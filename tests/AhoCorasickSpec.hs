-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq (rnf)
import Control.Monad (forM_, unless)
import Data.Foldable (foldl')
import Data.Text (Text)
import Data.Word (Word16)
import GHC.Stack (HasCallStack)
import Prelude hiding (replicate)
import Test.Hspec (Spec, Expectation, describe, it, shouldBe, hspec)
import Test.Hspec.Expectations (shouldMatchList, shouldSatisfy)
import Test.Hspec.QuickCheck (modifyMaxSuccess, modifyMaxSize, prop)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), forAll, forAllShrink, (==>))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Instances ()

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Internal.Search as TextSearch
import qualified Data.Text.Unsafe as TextUnsafe
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Gen as Gen

import Data.Text.AhoCorasick.Automaton (CaseSensitivity (..))

import qualified Data.Text.AhoCorasick.Automaton as Aho
import qualified Data.Text.AhoCorasick.Replacer as Replacer

instance Arbitrary CaseSensitivity where
  arbitrary = Gen.elements [CaseSensitive, IgnoreCase]

-- | Test that for a single needle which equals the haystack, we find a single
-- match. Does not apply to the empty needle.
needleIsHaystackMatches :: HasCallStack => Text -> Expectation
needleIsHaystackMatches needle =
  let
    needleUtf16 = Aho.unpackUtf16 needle
    len = Aho.lengthUtf16 needle
    matches = Aho.runText (Aho.build [(needleUtf16, ())]) needle
  in
    matches `shouldBe` [Aho.Match len ()]

ahoMatch :: [(Text, a)] -> Text -> [Aho.Match a]
ahoMatch needles haystack =
  let
    makeNeedle (text, value) = (Aho.unpackUtf16 text, value)
    needlesUtf16 = fmap makeNeedle needles
  in
    Aho.runText (Aho.build needlesUtf16) haystack

-- | Match without a payload, return only the match positions.
matchPositions :: [Text] -> Text -> [Int]
matchPositions needles haystack =
  let
    withUnit x = (x, ())
    matches = ahoMatch (fmap withUnit needles) haystack
  in
    fmap (Aho.codeUnitIndex . Aho.matchPos) matches

-- | `matchPositions` implemented naively in terms of Text's functionality,
-- which we assume to be correct.
naiveMatchPositions :: [Text] -> Text -> [Int]
naiveMatchPositions needles haystack =
  let
    prependMatch :: [Int] -> Text -> Int -> Text -> [Int]
    prependMatch matches needle offset haystackSlice =
      if Text.null haystack
        then matches
        -- Text.indices returns all non-overlapping occurrences of the needle,
        -- but we want the overlapping ones as well. So we only consider the
        -- first match, and then search again starting from one past the
        -- beginning of the match.
        else case TextSearch.indices needle haystackSlice of
          []  -> matches
          i:_ -> prependMatch (match : matches) needle offset' remainingHaystack
            where
              -- The match index is the index past the end, not the start index.
              match = offset + i + TextUnsafe.lengthWord16 needle
              offset' = offset + i + 1
              remainingHaystack = TextUnsafe.dropWord16 (i + 1) haystackSlice

    prependMatches matches needle = prependMatch matches needle 0 haystack
  in
    foldl' prependMatches [] needles

-- | Generate random needles and haystacks, such that the needles have a
-- reasonable probability of occuring in the haystack, which would hardly be the
-- case if we just generated random texts for all of them. We do this by first
-- generating a set of fragments, and then building the haystack and needles by
-- combining these fragments. By doing this, we also get a lot of partial
-- matches, where part of a needle does occur in the haystack, but the full
-- needle does not, and also needles with a shared prefix or suffix. This should
-- fully stress the possible transitions in the automaton.
arbitraryNeedlesHaystack :: Gen ([Text], Text)
arbitraryNeedlesHaystack = do
  let
    -- Prefer ascii just to have printable test cases, but do include the other
    -- generator to cover the entire range of code points.
    genChar = Gen.frequency
      [ (4, QuickCheck.arbitraryASCIIChar)
      , (1, QuickCheck.arbitrary)
      ]
    genNonEmptyText = do
      chars <- Gen.listOf1 genChar
      pure $ Text.pack chars

  fragments <- Gen.listOf1 $ Gen.resize 5 genNonEmptyText
  let
    genFragment = Gen.elements $ filter (not . Text.null) fragments
    genSmall = Gen.scale (`div` 3) $ Gen.listOf1 genFragment
    genBig = Gen.scale (* 4) $ Gen.listOf1 genFragment

  needles <- Gen.listOf1 (fmap Text.concat genSmall)
  haystack <- fmap Text.concat genBig
  pure (needles, haystack)

main :: IO ()
main = hspec $ describe "Data.Text.AhoCorasick" spec

spec :: Spec
spec = do
  modifyMaxSuccess (const 200) $ do
    describe "build" $ do
      prop "does not throw exceptions" $ \ (kv :: [([Word16], Int)]) ->
        rnf $ Aho.build kv

    describe "unpackUtf16" $ do
      it "unpacks code point U+437b8" $
        -- Note that 0x437b8 lies in the currently unassigned "Plane 5"; the
        -- code point does not currently exist, but that should not bother us.
        -- Check in Python: '\U000437b8'.encode('utf-16be')
        Aho.unpackUtf16 "\x000437b8" `shouldBe` [0xd8cd, 0xdfb8]

      it "unpacks adjacent nulls individually" $ do
        Aho.unpackUtf16 "c\NULe" `shouldBe` [99, 0, 101]
        Aho.unpackUtf16 "bc\NUL\NULe" `shouldBe` [98, 99, 0, 0, 101]

    describe "runText" $ do

      describe "when given a needle equal to the haystack" $ do

        it "reports a single match for a repeated character" $
          forM_ [1..128] $ \n ->
            needleIsHaystackMatches $ Text.replicate n "a"

        it "reports a single match for non-BMP data" $ do
          -- Include a few code points outside of the Basic Multilingual Plane,
          -- which require multiple code units to encode.
          needleIsHaystackMatches "\x000437b8suffix"
          needleIsHaystackMatches "aaa\359339aaa\95759aa\899256aa"

        prop "reports a single match for random needles" $ \needle ->
          not (Text.null needle) ==> needleIsHaystackMatches needle

      describe "when given a sliced text (with nonzero internal offset)" $ do

        it "still reports offset relative to the text start" $
          -- The match position should be relative to the start of the text "a".
          -- Even if this text is represented as a slice of "bbba" internally.
          matchPositions ["a"] (Text.dropWhile (== 'b') "bbba") `shouldMatchList` [1]

      describe "when given non-ascii inputs" $ do

        -- We have a special lookup table for transitions from the base state
        -- for the first 128 code units, which is always hit for ascii inputs.
        -- Also exercise the fallback code path with a different input.
        it "reports a match if the first haystack character is > U+7f" $ do
          matchPositions ["eclair"] "éclair" `shouldMatchList` []
          matchPositions ["éclair"] "éclair" `shouldMatchList` [6]
          matchPositions ["éclair"] "eclair" `shouldMatchList` []

        it "reports the correct UTF-16 index for surrogate pairs" $ do
          -- Note that the index after the match is 2, even though there is
          -- only a single code point. U+1d11e is encoded as two code units
          -- in UTF-16.
          matchPositions ["𝄞"] "𝄞" `shouldMatchList` [2]

          -- A leviating woman in business suit with dark skin tone needs a
          -- whopping 5 code points to encode, of which the first two need a
          -- surrogate pair in UTF-16, for a total of 7 code units.
          -- U+1f574: man in business suit levitating
          -- U+1f3ff: emoji modifier Fitzpatrick type-6
          -- U+200d:  zero width joiner
          -- U+2640:  female sign
          -- U+fe0f:  variation selector-16
          -- A peculiar feature of Unicode emoji, is that the male levivating
          -- man in business suit with dark skin tone is a substring of the
          -- levivating woman in business suit. And the levivating man in
          -- business suit without particular skin tone is a substring of that.
          matchPositions
            [ "\x1f574\x1f3ff\x200d\x2640\xfe0f"
            , "\x1f574\x1f3ff"
            , "\x1f574"
            ] "\x1f574\x1f3ff\x200d\x2640\xfe0f" `shouldMatchList` [2, 4, 7]

      describe "when given overlapping needles" $ do

        it "finds exactly all matches" $ do
          matchPositions ["foobar", "bar"] "foobar" `shouldMatchList` [6, 6]
          matchPositions ["foobarbaz", "bar"] "xfoobarbazy" `shouldMatchList` [10, 7]
          matchPositions ["foobar", "foo"] "xfoobarbazy" `shouldMatchList` [7, 4]

        it "keeps the value associated with a needle" $ do
          (fmap Aho.matchValue $ ahoMatch [("foo", 'A'), ("bar", 'B')] "foobar")
            `shouldMatchList` ['A', 'B']
          (fmap Aho.matchValue $ ahoMatch [("foo", 'A'), ("bar", 'B')] "foobaz")
            `shouldMatchList` ['A']
          (fmap Aho.matchValue $ ahoMatch [("foo", 'A'), ("bar", 'B')] "foebar")
            `shouldMatchList` ['B']

        it "reports both matches in case of a duplicate needle" $ do
          (fmap Aho.matchValue $ ahoMatch [("foo", 'A'), ("foo", 'B')] "foobar")
            `shouldMatchList` ['A', 'B']

        it "finds all quadratic matches" $
          forM_ ["a", "ab", "abc"] $ \baseText ->
            forM_ [1..33] $ \n ->
              let
                replicate k = Text.replicate k baseText
                needles = fmap replicate [1..n]
                matches = matchPositions needles (replicate n)
              in
                -- The needle of length 1 matches n times, the needle of length
                -- 2 matches n - 1 times, ..., the needle of length n matches
                -- once.
                length matches `shouldBe` sum [1..n]

      describe "when given partially overlapping needles" $ do

        it "finds exactly all matches" $ do
          matchPositions ["ab", "bcd"] "abccd" `shouldMatchList` [2]
          matchPositions ["abc","cde"] "abcdde" `shouldMatchList` [3]
          matchPositions ["c","c\NULe"] "c\NUL\NULe" `shouldMatchList` [1]
          -- The case below is a regression test; it did fail before; it would
          -- report a match at position 5 in addition to position 2.
          matchPositions ["bc","c\NULe"] "bc\NUL\NULe" `shouldMatchList` [2]

      describe "when given empyt needles" $ do

        it "does not report a match" $ do
          matchPositions [""] "" `shouldMatchList` []
          matchPositions [""] "foo" `shouldMatchList` []

      describe "when given random needles and haystacks" $ do

        prop "reports only infixes of the haystack" $
          QuickCheck.forAllShrink arbitraryNeedlesHaystack shrink $ \ (needles, haystack) ->
            let
              dup x = (x, x)
              matches = ahoMatch (fmap dup needles) haystack
              sliceMatch endPos len = Aho.unsafeSliceUtf16 (endPos - len) len haystack
            in
              -- Discard inputs for which there are no matches, to ensure we get
              -- enough coverage for the case where there are matches.
              not (null matches) ==>
                forM_ matches $ \ (Aho.Match pos needle) -> do
                  needle `shouldSatisfy` (`Text.isInfixOf` haystack)
                  sliceMatch pos (Aho.lengthUtf16 needle) `shouldBe` needle

        prop "reports all infixes of the haystack" $
          QuickCheck.forAllShrink arbitraryNeedlesHaystack shrink $ \ (needles, haystack) ->
            matchPositions needles haystack `shouldMatchList` naiveMatchPositions needles haystack

  describe "Char.toLower" $ do

    -- We test that Char.toLower maps the BMP onto itself, because this implies
    -- that changing casing code unit by code unit does not change the number of
    -- code units, which allows us to implement lowercasing in an optimized
    -- manner.
    it "maps the Basic Multilingual Plane onto itself" $
      let
        isSurrogate cu = cu >= 0xd800 && cu < 0xe000
      in
        forM_ [0 .. maxBound :: Aho.CodeUnit] $ \cu -> unless (isSurrogate cu) $
          let
            lower = Char.ord $ Char.toLower $ Char.chr $ fromIntegral cu
          in
            lower `shouldSatisfy` not . isSurrogate

  modifyMaxSize (const 10) $ describe "Replacer.run" $ do
    let
      genHaystack = fmap Text.pack $ Gen.listOf $ Gen.frequency [(40, Gen.elements "abAB"), (1, pure 'İ'), (1, arbitrary)]
      genNeedle = fmap Text.pack $ Gen.resize 3 $ Gen.listOf1 $ Gen.elements "abAB"
      genReplaces = Gen.listOf $ (,) <$> genNeedle <*> arbitrary
      shrinkReplaces = filter (not . any (\(needle, _) -> Text.null needle)) . shrink

      replace needles haystack = Replacer.run (Replacer.build CaseSensitive needles) haystack
      replaceIgnoreCase needles haystack = Replacer.run (Replacer.build IgnoreCase needles) haystack

    it "replaces all occurrences" $ do
      replace [("A", "B")] "AXAXB" `shouldBe` "BXBXB"
      replace [("A", "B"), ("X", "Y")] "AXAXB" `shouldBe` "BYBYB"
      replace [("aaa", ""), ("b", "c")] "aaabaaa" `shouldBe` "c"
      -- Have a few non-matching needles too.
      replace [("A", "B"), ("Q", "r"), ("Z", "")] "AXAXB" `shouldBe` "BXBXB"

    it "replaces only non-overlapping matches" $ do
      replace [("aa", "zz"), ("bb", "w")] "aaabbb" `shouldBe` "zzawb"
      replace [("aaa", "")] "aaaaa" `shouldBe` "aa"

    it "replaces all occurrences in priority order" $ do
      replace [("A", ""), ("BBBB", "bingo")] "BBABB" `shouldBe` "bingo"
      replace [("BB", ""), ("BBBB", "bingo")] "BBBB" `shouldBe` ""

    it "replaces needles that contain a surrogate pair" $
      replace [("\x1f574", "levivating man in business suit")]
        "the \x1f574" `shouldBe` "the levivating man in business suit"

    it "replaces all occurrences case-insensitively" $ do
      replaceIgnoreCase [("A", "B")] "AXAXB" `shouldBe` "BXBXB"
      replaceIgnoreCase [("A", "B")] "axaxb" `shouldBe` "BxBxb"
      replaceIgnoreCase [("a", "b")] "AXAXB" `shouldBe` "bXbXB"

      replaceIgnoreCase [("A", "B"), ("X", "Y")] "AXAXB" `shouldBe` "BYBYB"
      replaceIgnoreCase [("A", "B"), ("X", "Y")] "axaxb" `shouldBe` "BYBYb"
      replaceIgnoreCase [("a", "b"), ("x", "y")] "AXAXB" `shouldBe` "bybyB"

    it "matches replacements case-insensitively" $
      replaceIgnoreCase [("foo", "BAR"), ("bar", "BAZ")] "Foo" `shouldBe` "BAZ"

    it "matches replacements case-insensitively for non-ascii characters" $ do
      replaceIgnoreCase [("éclair", "lightning")] "Éclair" `shouldBe` "lightning"
      -- Note: U+0319 is an uppercase alpha, which looks exactly like A, but it
      -- is a different code point.
      replaceIgnoreCase [("bèta", "α"), ("\x0391", "alpha")] "BÈTA" `shouldBe` "alpha"

    it "matches surrogate pairs case-insensitively" $ do
      -- We can't lowercase a levivating man in business suit, but that should
      -- not affect whether we match it or not.
      replaceIgnoreCase [("\x1f574", "levivating man in business suit")]
        "the \x1f574" `shouldBe` "the levivating man in business suit"

    prop "satisfies (run . compose a b) == (run b (run a))" $
      forAllShrink genHaystack shrink $ \haystack ->
      forAll arbitrary $ \case_ ->
      forAllShrink genReplaces shrinkReplaces $ \replaces1 ->
      forAllShrink genReplaces shrinkReplaces $ \replaces2 ->
      let
        rm1 = Replacer.build case_ replaces1
        rm2 = Replacer.build case_ replaces2
        Just rm12 = Replacer.compose rm1 rm2
      in
        Replacer.run rm2 (Replacer.run rm1 haystack)
          `shouldBe` Replacer.run rm12 haystack

    prop "is identity for empty needles" $ \case_ haystack ->
      let replacerId = Replacer.build case_ []
      in Replacer.run replacerId haystack `shouldBe` haystack

    prop "is equivalent to sequential Text.replace calls" $
      forAllShrink genHaystack shrink $ \haystack ->
      forAllShrink genReplaces shrinkReplaces $ \replaces ->
      let
        replacer = Replacer.build CaseSensitive replaces
        replaceText agg (needle, replacement) = Text.replace needle replacement agg
        expected = foldl' replaceText haystack replaces
      in
        Replacer.run replacer haystack `shouldBe` expected
