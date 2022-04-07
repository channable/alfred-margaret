{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Text.BoyerMooreSpec
    ( spec
    ) where

import Control.DeepSeq (rnf)
import Control.Monad (forM_)
import Data.Foldable (for_)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Prelude hiding (replicate)
import Test.Hspec (Expectation, Spec, describe, it, parallel, shouldBe)
import Test.Hspec.Expectations (shouldMatchList, shouldSatisfy)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), forAll, forAllShrink, (==>))
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Instances ()

import qualified Data.Text as Text
import qualified Data.Text.Internal.Search as TextSearch
import qualified Data.Text.Unsafe as TextUnsafe
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Gen as Gen

import Data.Text.BoyerMoore.Automaton (CaseSensitivity (..))
import Data.Text.Orphans ()

import qualified Data.Text.AhoCorasick.Replacer as AhoReplacer
import qualified Data.Text.BoyerMoore.Automaton as BoyerMoore
import qualified Data.Text.BoyerMoore.Replacer as Replacer
import qualified Data.Text.BoyerMoore.Searcher as Searcher
import qualified Data.Text.Utf16 as Utf16

-- | Test that for a single needle which equals the haystack, we find a single
-- match. Does not apply to the empty needle.
needleIsHaystackMatches :: HasCallStack => Text -> Expectation
needleIsHaystackMatches needle =
  let
    prependMatch ms match = BoyerMoore.Step (Utf16.codeUnitIndex match : ms)
    matches = BoyerMoore.runText [] prependMatch (BoyerMoore.buildAutomaton needle) needle
  in
    matches `shouldBe` [0]

boyerMatch :: Text -> Text -> [Int]
boyerMatch needle haystack =
  let
    prependMatch matches match = BoyerMoore.Step (Utf16.codeUnitIndex match : matches)
  in
    BoyerMoore.runText [] prependMatch (BoyerMoore.buildAutomaton needle) haystack

-- | Match without a payload, return only the match positions.
matchEndPositions :: Text -> Text -> [Int]
matchEndPositions needle haystack =
  let
    matches = boyerMatch needle haystack
  in
    fmap (Utf16.codeUnitIndex (Utf16.lengthUtf16 needle) +) matches

-- | `matchEndPositions` implemented naively in terms of Text's functionality,
-- which we assume to be correct.
naiveMatchPositions :: Text -> Text -> [Int]
naiveMatchPositions needle haystack =
  map toEndPos $ TextSearch.indices needle haystack
  where
    toEndPos index = TextUnsafe.lengthWord16 needle + index

-- | Generate random needles and haystacks, such that the needles have a
-- reasonable probability of occuring in the haystack, which would hardly be the
-- case if we just generated random texts for all of them. We do this by first
-- generating a set of fragments, and then building the haystack and needles by
-- combining these fragments. By doing this, we also get a lot of partial
-- matches, where part of a needle does occur in the haystack, but the full
-- needle does not, and also needles with a shared prefix or suffix. This should
-- fully stress the possible transitions in the automaton.
arbitraryNeedleHaystack :: Gen (Text, Text)
arbitraryNeedleHaystack = do
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

  needle <- fmap Text.concat genSmall
  haystack <- fmap Text.concat genBig
  pure (needle, haystack)

spec :: Spec
spec = parallel $ modifyMaxSuccess (const 200) $ do
  describe "build" $ do
    prop "does not throw exceptions" $ \ (pat :: Text) ->
      rnf $ BoyerMoore.buildAutomaton pat

  describe "runText" $ do

    describe "when given a needle equal to the haystack" $ do

      it "reports a single match for a repeated character" $
        forM_ [1..128] $ \n ->
          needleIsHaystackMatches $ Text.replicate n "a"

      it "reports a single match for non-BMP data" $ do
        -- Include a few code points outside of the Basic Multilingual Plane,
        -- which require multible code units to encode.
        needleIsHaystackMatches "\x000437b8suffix"
        needleIsHaystackMatches "aaa\359339aaa\95759aa\899256aa"

      prop "reports a single match for random needles" $ \needle ->
        not (Text.null needle) ==> needleIsHaystackMatches needle

    describe "when given a sliced text (with nonzero internal offset)" $ do

      it "still reports offset relative to the text start" $
        -- The match position should be relative to the start of the text "a".
        -- Even if this text is represented as a slice of "bbba" internally.
        matchEndPositions "a" (Text.dropWhile (== 'b') "bbba") `shouldMatchList` [1]

    describe "when given non-ascii inputs" $ do

      -- We have a special lookup table for bad character shifts for
      -- the first 128 code units, which is always hit for ascii inputs.
      -- Also exercise the fallback code path with a different input.
      it "reports a match if the haystack contains a character > U+7f" $ do
        matchEndPositions "eclair" "éclaireclair" `shouldMatchList` [12]
        matchEndPositions "éclair" "éclaireclair" `shouldMatchList` [6]
        matchEndPositions "éclair" "eclairéclair" `shouldMatchList` [12]

      it "reports the correct UTF-16 index for surrogate pairs" $ do
        -- Note that the index after the match is 2, even though there is
        -- only a single code point. U+1d11e is encoded as two code units
        -- in UTF-16.
        matchEndPositions "𝄞" "𝄞" `shouldMatchList` [2]

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
        let
          examples =
            [ ("\x1f574\x1f3ff\x200d\x2640\xfe0f", 7)
            , ("\x1f574\x1f3ff", 4)
            , ("\x1f574", 2)
            ]
        for_ examples $ \(needle, endPos) ->
          matchEndPositions needle "\x1f574\x1f3ff\x200d\x2640\xfe0f" `shouldMatchList` [endPos]

    describe "when given empty needle" $ do

      it "does not report a match" $ do
        matchEndPositions "" "" `shouldMatchList` []
        matchEndPositions "" "foo" `shouldMatchList` []

    describe "kitchen sink" $ do
      it "kitchen sinks" $ do
        matchEndPositions "\"\SO]JL\"" "aaaaa\"\SO]JL\"" `shouldMatchList` [11]
        matchEndPositions "\"X]JL\"" "aaaaa\"X]JL\"" `shouldMatchList` [11]

    describe "when given random needles and haystacks" $ do

      prop "reports only infixes of the haystack" $
        QuickCheck.forAllShrink arbitraryNeedleHaystack shrink $ \ (needle, haystack) ->
          let
            matches = boyerMatch needle haystack
            sliceMatch startPos len = Utf16.unsafeSliceUtf16 startPos len haystack
          in
            forM_ matches $ \pos -> do
              needle `shouldSatisfy` (`Text.isInfixOf` haystack)
              sliceMatch (Utf16.CodeUnitIndex pos) (Utf16.lengthUtf16 needle) `shouldBe` needle

      prop "reports all infixes of the haystack" $
        QuickCheck.forAllShrink arbitraryNeedleHaystack shrink $ \ (needle, haystack) ->
          matchEndPositions needle haystack `shouldMatchList` naiveMatchPositions needle haystack

  describe "replaceSingleLimited" $ do

    prop "is equivalent to Aho-Corasick replacer with a single needle" $
      forAllShrink arbitraryNeedleHaystack shrink $ \(needle, haystack) ->
      forAllShrink arbitrary shrink $ \replacement ->
      forAll arbitrary $ \case_ ->
      let
        expected = AhoReplacer.run (AhoReplacer.build case_ [(needle, replacement)]) haystack

        auto = BoyerMoore.buildAutomaton $ case case_ of
          IgnoreCase -> Utf16.lowerUtf16 needle
          CaseSensitive -> needle

        actual = Replacer.replaceSingleLimited case_ auto replacement haystack maxBound
      in
        actual `shouldBe` Just expected

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
      prop "is equivalent to disjunction of Text.isInfixOf calls*" $ \ (needles :: [Text]) (haystack :: Text) ->
        let
          searcher = Searcher.build CaseSensitive needles
          test needle =
            not (Text.null needle) && needle `Text.isInfixOf` haystack
        in
          Searcher.containsAny searcher haystack `shouldBe` any test needles

    describe "containsAll" $ do

      prop "is equivalent to conjunction of Text.isInfixOf calls*" $ \ (needles :: [Text]) (haystack :: Text) ->
        let
          searcher = Searcher.buildNeedleIdSearcher CaseSensitive needles
          test needle =
            not (Text.null needle) && needle `Text.isInfixOf` haystack
        in
          Searcher.containsAll searcher haystack `shouldBe` all test needles

      prop "performs case-insensitive search as well" $ \ (needles :: [Text]) (haystack :: Text) ->
        let
          lowerNeedles = map Utf16.lowerUtf16 needles
          lowerHaystack = Utf16.lowerUtf16 haystack
          searcher = Searcher.buildNeedleIdSearcher IgnoreCase lowerNeedles
          test needle =
            not (Text.null needle) && needle `Text.isInfixOf` lowerHaystack
        in
          Searcher.containsAll searcher haystack `shouldBe` all test lowerNeedles
