-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Text.Utf8.BoyerMoore.Replacer
    ( -- Replacer
      replaceSingleLimited
    ) where

import Data.Text.CaseSensitivity (CaseSensitivity (..))
import Data.Text.Utf8 (Text)
import Data.Text.Utf8.BoyerMoore.Automaton (Automaton, CodeUnitIndex)

import qualified Data.Text.Utf8 as Text
import qualified Data.Text.Utf8 as Utf8
import qualified Data.Text.Utf8.BoyerMoore.Automaton as BoyerMoore

-- | Replace all occurrences matched by the Boyer-Moore automaton
-- with the given replacement text in some haystack.
replaceSingleLimited
  :: CaseSensitivity
  -- ^ In case of 'IgnoreCase', the automaton must have been created with a lower-case needle
  -> Automaton -- ^ Matches the needles
  -> Text -- ^ Replacement string
  -> Text -- ^ Haystack
  -> CodeUnitIndex -- ^ Maximum number of code units in the returned text
  -> Maybe Text
replaceSingleLimited caseSensitivity needle replacement haystack maxLength
  | needleLength == 0 = Just $ if haystackLength == 0 then replacement else haystack
  | otherwise = finish $ case caseSensitivity of
      CaseSensitive -> BoyerMoore.runText initial foundMatch needle haystack
      IgnoreCase -> BoyerMoore.runLower initial foundMatch needle haystack
  where
    needleLength = BoyerMoore.patternLength needle
    haystackLength = Utf8.lengthUtf8 haystack
    replacementLength = Utf8.lengthUtf8 replacement

    initial = ReplaceState
      { rsChunks = []
      , rsPreviousMatchEnd = 0
      , rsLength = 0
      }

    foundMatch rs matchStart =
      let
        matchEnd = matchStart + needleLength

        -- Slice the part of the haystack between the end of the previous match
        -- and the start of the current match
        haystackPartLength = matchStart - rsPreviousMatchEnd rs
        haystackPart = Utf8.unsafeSliceUtf8 (rsPreviousMatchEnd rs) haystackPartLength haystack

        -- Add the preceding part of the haystack and the replacement in reverse
        -- order to the chunk list (all chunks will be reversed at once in the final step).
        newChunks = replacement : haystackPart : rsChunks rs
        newLength = replacementLength + haystackPartLength + rsLength rs

        newState = ReplaceState
          { rsChunks = newChunks
          , rsPreviousMatchEnd = matchEnd
          , rsLength = newLength
          }
      in
        if newLength > maxLength
          then BoyerMoore.Done newState
          else BoyerMoore.Step newState

    finish rs =
      let
        -- Slice the remaining part of the haystack from the end of the last match
        -- to the end of the haystack.
        haystackPartLength = haystackLength - rsPreviousMatchEnd rs
        finalChunks
            = Utf8.unsafeSliceUtf8 (rsPreviousMatchEnd rs) haystackPartLength haystack
            : rsChunks rs
        finalLength = rsLength rs + haystackPartLength
      in
        if finalLength > maxLength
          then Nothing
          else Just $ Text.concat $ reverse finalChunks

-- | Internal accumulator state for performing a replace while stepping an automaton
data ReplaceState = ReplaceState
  { rsChunks :: [Text]
    -- ^ Chunks of the final text, in reverse order so that we can efficiently prepend
  , rsPreviousMatchEnd :: !CodeUnitIndex
    -- ^ Index one past the end of the last match.
  , rsLength :: !CodeUnitIndex
    -- ^ Length of the newly build string so far, measured in CodeUnits
  }
