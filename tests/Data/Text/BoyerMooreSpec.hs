{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Text.BoyerMooreSpec
    ( spec
    ) where

import Control.DeepSeq (rnf)
import Control.Monad (forM_)
import Data.Foldable (for_)
import GHC.Stack (HasCallStack)
import Prelude hiding (replicate)
import Test.Hspec (Expectation, Spec, describe, it, parallel, shouldBe)
import Test.Hspec.Expectations (shouldMatchList, shouldSatisfy)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), forAllShrink, (==>))
import Test.QuickCheck.Instances ()

import qualified Test.QuickCheck as QuickCheck

import Data.Text.CaseSensitivity (CaseSensitivity (..))
import Data.Text.TestInstances (arbitraryNeedleHaystack)
import Data.Text.Utf8 (Text)

import qualified Data.Text.Utf8 as Text
import qualified Data.Text.Utf8 as TextSearch
import qualified Data.Text.Utf8 as Utf8
import qualified Data.Text.AhoCorasick.Replacer as AhoReplacer
import qualified Data.Text.BoyerMoore.Automaton as BoyerMoore
import qualified Data.Text.BoyerMoore.Replacer as Replacer
import qualified Data.Text.BoyerMoore.Searcher as Searcher

-- | Test that for a single needle which equals the haystack, we find a single
-- match. Does not apply to the empty needle.
needleIsHaystackMatches :: HasCallStack => Text -> Expectation
needleIsHaystackMatches needle =
  let
    prependMatch ms match = BoyerMoore.Step (Utf8.codeUnitIndex match : ms)
    matches = BoyerMoore.runText [] prependMatch (BoyerMoore.buildAutomaton needle) needle
  in
    matches `shouldBe` [0]

boyerMatch :: Text -> Text -> [Int]
boyerMatch needle haystack =
  let
    prependMatch matches match = BoyerMoore.Step (Utf8.codeUnitIndex match : matches)
  in
    BoyerMoore.runText [] prependMatch (BoyerMoore.buildAutomaton needle) haystack

-- | Match without a payload, return only the match positions.
matchEndPositions :: Text -> Text -> [Int]
matchEndPositions needle haystack =
  let
    matches = boyerMatch needle haystack
  in
    fmap (Utf8.codeUnitIndex (Utf8.lengthUtf8 needle) +) matches

-- | `matchEndPositions` implemented naively in terms of Text's functionality,
-- which we assume to be correct.
naiveMatchPositions :: Text -> Text -> [Int]
naiveMatchPositions needle haystack =
  map toEndPos $ TextSearch.indices needle haystack
  where
    toEndPos index = Utf8.codeUnitIndex (Utf8.lengthUtf8 needle) + index

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
      -- The code point é is encoded as two code units in UTF-8.
      -- 0             7          13
      -- │             │           │
      -- ▼             ▼           ▼
      -- ┌───┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐
      -- │ é │c│l│a│i│r│e│c│l│a│i│r│ Code Points
      -- ├─┬─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┼─┤
      -- │ │ │ │ │ │ │ │ │ │ │ │ │ │ Code Units (Bytes)
      -- └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘
      it "reports a match if the haystack contains a character > U+7f" $ do
        matchEndPositions "eclair" "éclaireclair" `shouldMatchList` [13]
        matchEndPositions "éclair" "éclaireclair" `shouldMatchList` [7]
        matchEndPositions "éclair" "eclairéclair" `shouldMatchList` [13]

      it "reports the correct code unit index for complex characters" $ do
        -- Note that the index after the match is 4, even though there is
        -- only a single code point. U+1d11e is encoded as four code units:
        -- in UTF-8:
        -- 0       4
        -- │       │
        -- ▼       ▼
        -- ┌───────┐
        -- │   𝄞   │ Code Points
        -- ├─┬─┬─┬─┤
        -- │ │ │ │ │ Code Units (Bytes)
        -- └─┴─┴─┴─┘
        matchEndPositions "𝄞" "𝄞" `shouldMatchList` [4]

        -- A levitating woman in business suit with dark skin tone needs a
        -- whopping 5 code points to encode. The first two need 4 code units each to encode,
        -- the remaining three need 3 code units each for a total of 17 code units:
        -- 0       4       8                17
        -- │       │       │                 │
        -- ▼       ▼       ▼                 ▼
        -- ┌───────┬───────┬─────┬─────┬─────┐
        -- │   1   │   2   │  3  │  4  │  5  │ Code Points
        -- ├─┬─┬─┬─┼─┬─┬─┬─┼─┬─┬─┼─┬─┬─┼─┬─┬─┤
        -- │ │ │ │ │ │ │ │ │ │ │ │ │ │ │ │ │ │ Code Units (Bytes)
        -- └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘
        -- 1. U+1f574: man in business suit levitating (🕴)
        -- 2. U+1f3ff: emoji modifier Fitzpatrick type-6
        -- 3. U+200d:  zero width joiner
        -- 4. U+2640:  female sign (♀)
        -- 5. U+fe0f:  variation selector-16
        -- A peculiar feature of Unicode emoji, is that the male levivating
        -- man in business suit with dark skin tone is a substring of the
        -- levivating woman in business suit. And the levivating man in
        -- business suit without particular skin tone is a substring of that.
        let
          examples =
            [ ("\x1f574\x1f3ff\x200d\x2640\xfe0f", 17)
            , ("\x1f574\x1f3ff", 8)
            , ("\x1f574", 4)
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
            sliceMatch startPos len = Utf8.unsafeSliceUtf8 startPos len haystack
          in
            forM_ matches $ \pos -> do
              needle `shouldSatisfy` (`Text.isInfixOf` haystack)
              sliceMatch (Utf8.CodeUnitIndex pos) (Utf8.lengthUtf8 needle) `shouldBe` needle

      prop "reports all infixes of the haystack" $
        QuickCheck.forAllShrink arbitraryNeedleHaystack shrink $ \ (needle, haystack) ->
          matchEndPositions needle haystack `shouldMatchList` naiveMatchPositions needle haystack

  describe "replaceSingleLimited" $ do

    prop "is equivalent to Aho-Corasick replacer with a single needle" $
      forAllShrink arbitraryNeedleHaystack shrink $ \(needle, haystack) ->
      forAllShrink arbitrary shrink $ \replacement ->
      let
        expected = AhoReplacer.run (AhoReplacer.build CaseSensitive [(needle, replacement)]) haystack

        auto = BoyerMoore.buildAutomaton needle

        actual = Replacer.replaceSingleLimited auto replacement haystack maxBound
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
          searcher = Searcher.build needles
          test needle =
            not (Text.null needle) && needle `Text.isInfixOf` haystack
        in
          Searcher.containsAny searcher haystack `shouldBe` any test needles

    describe "containsAll" $ do
      prop "is equivalent to conjunction of Text.isInfixOf calls*" $ \ (needles :: [Text]) (haystack :: Text) ->
        let
          searcher = Searcher.buildNeedleIdSearcher needles
          test needle =
            not (Text.null needle) && needle `Text.isInfixOf` haystack
        in
          Searcher.containsAll searcher haystack `shouldBe` all test needles
