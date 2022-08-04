{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.BoyerMooreCISpec
  ( spec
  ) where


import Control.Monad (forM_)
import Test.Hspec (Spec, describe, it, shouldBe, parallel)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Data.Text (Text)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), forAllShrink)
import Test.QuickCheck.Instances ()

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Utf8 as Utf8
import qualified Test.QuickCheck as QuickCheck

import Data.Text.CaseSensitivity (CaseSensitivity (..))
import Data.Text.TestInstances (arbitraryAlphabet, arbitraryFragment, arbitraryNeedleHaystack,
                                arbitraryNeedlesHaystack)

import qualified Data.Text.AhoCorasick.Replacer as AhoReplacer
import qualified Data.Text.BoyerMooreCI.Automaton as BoyerMooreCI
import qualified Data.Text.BoyerMooreCI.Searcher as Searcher
import qualified Data.Text.BoyerMooreCI.Replacer as Replacer


spec :: Spec
spec = parallel $ modifyMaxSuccess (const 200) $ do

  describe "automaton" $ do

    it "works for some basic examples" $ do
      matchPositions "a" "abca" `shouldBe` [(0,0), (3,3)]
      matchPositions "a" "ABCA" `shouldBe` [(0,0), (3,3)]
      matchPositions "abc" "abca" `shouldBe` [(0,2)]
      matchPositions "abc" "ABCA" `shouldBe` [(0,2)]
      matchPositions "bc" "abca" `shouldBe` [(1,2)]
      matchPositions "bc" "ABCA" `shouldBe` [(1,2)]

    it "does not yield overlapping matches" $ do
      matchPositions "aba" "abababa" `shouldBe` [(0,2), (4,6)]
      matchPositions "aba" "ABaBaBA" `shouldBe` [(0,2), (4,6)]

    it "does not work with uppercase needles" $ do
      matchPositions "A" "aaaa" `shouldBe` []
      matchPositions "A" "AAAA" `shouldBe` []

    it "works with cyrillic characters" $ do
      -- Cyrillic characters are all two bytes.
      -- The match positions are byte indices (not char indices).
      matchPositions "п" "ипсум" `shouldBe` [(2,3)]
      matchPositions "п" "ИПСУМ" `shouldBe` [(2,3)]

      matchPositions "лорем" "Лорем" `shouldBe` [(0,9)]
      matchTexts "лорем" "Лорем" `shouldBe` ["Лорем"]

      matchPositions "лорем" "ЛОРЕМ" `shouldBe` [(0,9)]
      matchTexts "лорем" "ЛОРЕМ" `shouldBe` ["ЛОРЕМ"]

      -- This is an interesting case for badCharLookup, because the mismatch
      -- happens at "с" which is the first character in the needle.
      matchTexts "сит" "итсит" `shouldBe` ["сит"]
      matchTexts "сит" "ИТСИТ" `shouldBe` ["СИТ"]

    it "works with mixed byte lengths" $ do
      -- Space is 1 byte
      matchTexts "сит" "Лор сит амет" `shouldBe` ["сит"]
      matchTexts "сит" "Лорем ипсум долор сит амет" `shouldBe` ["сит"]
      matchTexts "сит" "ЛОРЕМ ИПСУМ ДОЛОР СИТ АМЕТ" `shouldBe` ["СИТ"]

      matchTexts "💩b" "ЛОРЕМab𝄞💩𝄞ДОЛab💩baåÅÅ𝄞𝄞ßẞ" `shouldBe` ["💩b"]
      matchTexts "𝄞" "ЛОРЕМab𝄞💩𝄞ДОЛab💩baåÅÅ𝄞𝄞ßẞ" `shouldBe` ["𝄞","𝄞","𝄞","𝄞"]
      matchTexts "a" "ЛОРЕМab𝄞💩𝄞ДОЛab💩baåÅÅ𝄞𝄞ßẞ" `shouldBe` ["a","a","a"]

    it "works with ⱥ and ⱦ" $ do
      -- The letters ⱥ and ⱦ are 3 UTF8 bytes, but have unlowerings Ⱥ and Ⱦ of 2 bytes
      matchPositions "ⱥⱦⱥⱦⱥⱦ" "ⱥⱦⱥⱦⱥⱦ" `shouldBe` [(0, 17)]
      matchTexts "ⱥⱦⱥⱦⱥⱦ" "ⱥⱦⱥⱦⱥⱦ" `shouldBe` ["ⱥⱦⱥⱦⱥⱦ"]
      matchPositions "ⱥⱦⱥⱦⱥⱦ" "ȺȾȺȾȺȾ" `shouldBe` [(0, 11)]
      matchTexts "ⱥⱦⱥⱦⱥⱦ" "ȺȾȺȾȺȾ" `shouldBe` ["ȺȾȺȾȺȾ"]

      matchPositions "ⱥⱦⱥⱦⱥⱦ" "ȺⱦⱥȾⱥȾ" `shouldBe` [(0, 14)]
      matchTexts "ⱥⱦⱥⱦⱥⱦ" "ȺⱦⱥȾⱥȾ" `shouldBe` ["ȺⱦⱥȾⱥȾ"]

    describe "with a needle equal to the haystack" $ do

      it "reports a single match for a repeated character" $
        forM_ [1..128] $ \n ->
          let needle = Text.replicate n "a" in
          matchPositions needle needle `shouldBe` [(0, Utf8.lengthUtf8 needle-1)]

      prop "reports a single match for any arbitrary text fragment" $
        QuickCheck.forAll (arbitraryAlphabet >>= arbitraryFragment) $ \text ->
          let needle = Utf8.lowerUtf8 text in
          matchPositions needle text `shouldBe` [(0, Utf8.lengthUtf8 text-1)]

    describe "with sliced text (using nonzero internal offset)" $ do

      it "still reports offset relative to the text start" $
        -- The match position should be relative to the start of the text "a".
        -- Even if this text is represented as a slice of "bbba" internally.
        matchPositions "a" (Text.dropWhile (== 'b') "bbba") `shouldBe` [(0, 0)]

    it "matches ß and ẞ" $ do
      matchTexts "groß" "Großfräsmaschinenöffnungstür" `shouldBe` ["Groß"]
      matchTexts "groß" "GROẞFRÄSMASCHINENÖFFNUNGSTÜR" `shouldBe` ["GROẞ"]
      matchTexts "öffnung" "Großfräsmaschinenöffnungstür" `shouldBe` ["öffnung"]
      matchTexts "öffnung" "GROẞFRÄSMASCHINENÖFFNUNGSTÜR" `shouldBe` ["ÖFFNUNG"]


  describe "minimumSkipForCodePoint" $
    it "should match the reference implementation" $ do
      forM_ [minBound..maxBound] $ \c ->
        BoyerMooreCI.minimumSkipForCodePoint c `shouldBe` refMinimumSkipForCodePoint c


  describe "Searcher" $ do
    describe "containsAny" $ do

      -- For the edge case where a needle is the empty string,
      -- 'Text.isInfixOf' and 'Searcher.containsAny' are different:
      --
      -- @
      -- Text.isInfixOf "" "abc" == True /= False == Searcher.containsAny (Searcher.build [""]) "abc"
      -- @
      --
      -- However, at this point we probably shouldn't break this property.
      prop "is equivalent to disjunction of Text.isInfixOf calls*" $ do
        QuickCheck.forAllShrink arbitraryNeedlesHaystack shrink $ \(needles, haystack) -> do
          let
            lneedles = map Utf8.lowerUtf8 needles  -- needles must be lowercase
            searcher = Searcher.build lneedles
            test needle =
              not (Text.null needle) && needle `Text.isInfixOf` (Utf8.lowerUtf8 haystack)
          Searcher.containsAny searcher haystack `shouldBe` any test lneedles

    describe "containsAll" $ do
      prop "is equivalent to conjunction of Text.isInfixOf calls*" $ do
        QuickCheck.forAllShrink arbitraryNeedlesHaystack shrink $ \(needles, haystack) -> do
          let
            lneedles = map Utf8.lowerUtf8 needles  -- needles must be lowercase
            searcher = Searcher.buildNeedleIdSearcher lneedles
            test needle =
              not (Text.null needle) && needle `Text.isInfixOf` (Utf8.lowerUtf8 haystack)
          Searcher.containsAll searcher haystack `shouldBe` all test lneedles

  describe "Replacer" $ do
    describe "replaceSingleLimited" $ do

      prop "is equivalent to Aho-Corasick replacer with a single needle" $
        forAllShrink arbitraryNeedleHaystack shrink $ \(needle, haystack) ->
        forAllShrink arbitrary shrink $ \replacement ->
        let
          lneedle = Utf8.lowerUtf8 needle
          expected =
            AhoReplacer.run (AhoReplacer.build IgnoreCase [(lneedle, replacement)]) haystack
          auto = BoyerMooreCI.buildAutomaton lneedle
          actual = Replacer.replaceSingleLimited auto replacement haystack maxBound
        in
          actual `shouldBe` Just expected

-- Reference implementation for BoyerMooreCI.minimumSkipForCodePoint
refMinimumSkipForCodePoint :: Utf8.CodePoint -> Utf8.CodeUnitIndex
refMinimumSkipForCodePoint cp =
  let codePointLength = length . Utf8.unicode2utf8 . Char.ord in
  case Utf8.unlowerCodePoint cp of
    [] ->
      -- Input is upper case, so this is undefined behaviour but we match what the real
      -- implementation does:
      Utf8.CodeUnitIndex $ codePointLength cp
    ucs -> Utf8.CodeUnitIndex $ minimum $ map codePointLength ucs


-- | Return indices of the first and last byte of every match
matchPositions :: Text -> Text -> [(Utf8.CodeUnitIndex, Utf8.CodeUnitIndex)]
matchPositions needle =
  let
    !automaton = BoyerMooreCI.buildAutomaton needle
    prependMatch matches matchStart matchEnd =
      BoyerMooreCI.Step ((matchStart, matchEnd) : matches)
  in
    \haystack -> reverse $ BoyerMooreCI.runText [] prependMatch automaton haystack

positionText :: Text -> (Utf8.CodeUnitIndex, Utf8.CodeUnitIndex) -> Text
positionText haystack (firstByte, lastByte) =
  let len = lastByte - firstByte + 1  -- length is 1 if firstByte==lastByte
  in Utf8.unsafeSliceUtf8 firstByte len haystack

matchTexts :: Text -> Text -> [Text]
matchTexts needle haystack =
  map (positionText haystack) $ matchPositions needle haystack
