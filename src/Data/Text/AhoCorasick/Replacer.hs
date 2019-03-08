-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

-- See Automaton.hs for why these GHC flags are here.
{-# OPTIONS_GHC -fllvm -O2 -optlo=-O3 -optlo=-tailcallelim -fno-ignore-asserts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Implements sequential string replacements based on the Aho-Corasick algorithm.
module Data.Text.AhoCorasick.Replacer
  ( -- * State machine
    Replacer (..)
  , build
  , compose
  , run
  , runWithLimit
  , Needle
  , Replacement
  , Payload (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Text as Text

import Data.Text.AhoCorasick.Automaton (CaseSensitivity (..), CodeUnitIndex)
import Data.Text.AhoCorasick.Searcher (Searcher)

import qualified Data.Text.AhoCorasick.Automaton as Aho
import qualified Data.Text.AhoCorasick.Searcher as Searcher

-- | Descriptive type alias for strings to search for.
type Needle = Text

-- | Descriptive type alias for replacements.
type Replacement = Text

-- | Priority of a needle. Higher integers indicate higher priorities.
-- Replacement order is such that all matches of priority p are replaced before
-- replacing any matches of priority q where p > q.
type Priority = Int

data Payload = Payload
  { needlePriority    :: {-# UNPACK #-} !Priority
  , needleLength      :: {-# UNPACK #-} !CodeUnitIndex
  , needleReplacement :: !Replacement
  } deriving (Eq, Generic, Hashable, NFData, Show)

-- | A state machine used for efficient replacements with many different needles.
data Replacer = Replacer
  { replacerCaseSensitivity :: CaseSensitivity
  , replacerSearcher :: Searcher Payload
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)

-- | Build an Aho-Corasick automaton that can be used for performing fast
-- sequential replaces.
--
-- Case-insensitive matching performs per-letter language-agnostic case folding.
-- Therefore, it will work in most cases, but not in languages where case folding
-- depends on the context of the character in question.
--
-- We need to revisit this algorithm when we want to implement full Unicode
-- support.
build :: CaseSensitivity -> [(Needle, Replacement)] -> Replacer
build caseSensitivity replaces = Replacer caseSensitivity searcher
  where
    searcher = Searcher.buildWithValues $ zipWith mapNeedle [0..] replaces
    mapNeedle i (needle, replacement) =
      let
        needle' = case caseSensitivity of
          CaseSensitive -> needle
          IgnoreCase -> Aho.lowerUtf16 needle
      in
        -- Note that we negate i: earlier needles have a higher priority. We
        -- could avoid it and define larger integers to be lower priority, but
        -- that made the terminology in this module very confusing.
        (needle', Payload (-i) (Aho.lengthUtf16 needle') replacement)

-- | Return the composition `replacer2` after `replacer1`, if they have the same
-- case sensitivity. If the case sensitivity differs, Nothing is returned.
compose :: Replacer -> Replacer -> Maybe Replacer
compose (Replacer case1 searcher1) (Replacer case2 searcher2)
  | case1 /= case2 = Nothing
  | otherwise =
      let
        -- Replace the priorities of the second machine, so they all come after
        -- the first.
        renumber i (needle, Payload _ len replacement) = (needle, Payload (-i) len replacement)
        needles1 = Searcher.needles searcher1
        needles2 = Searcher.needles searcher2
        searcher = Searcher.buildWithValues $ zipWith renumber [0..] (needles1 ++ needles2)
      in
        Just $ Replacer case1 searcher

-- A match collected while running replacements. It is isomorphic to the Match
-- reported by the automaton, but the data is arranged in a more useful way:
-- as the start index and length of the match, and the replacement.
data Match = Match !CodeUnitIndex !CodeUnitIndex !Text deriving (Eq, Ord, Show)

-- | Apply replacements of all matches. Assumes that the matches are ordered by
-- match position, and that no matches overlap.
replace :: [Match] -> Text -> Text
replace matches haystack = Text.concat $ go 0 matches haystack
  where
    -- At every match, cut the string into three pieces, removing the match.
    -- Because a Text is a buffer pointer and (offset, length), cutting does not
    -- involve string copies. Only at the very end we piece together the strings
    -- again, so Text can allocate a buffer of the right length and memcpy the
    -- parts into the new target string.
    -- If `k` is a code unit index into the original text, then `k - offset`
    -- is an index into `remainder`. In other words, `offset` is the index into
    -- the original text where `remainder` starts.
    go :: CodeUnitIndex -> [Match] -> Text -> [Text]
    go !_offset [] remainder = [remainder]
    go !offset ((Match pos len replacement) : ms) remainder =
      let
        (prefix, suffix) = Aho.unsafeCutUtf16 (pos - offset) len remainder
      in
        prefix : replacement : go (pos + len) ms suffix

-- | Compute the length of the string resulting from applying the replacements.
replacementLength :: [Match] -> Text -> CodeUnitIndex
replacementLength matches initial  = go matches (Aho.lengthUtf16 initial)
  where
    go [] !acc = acc
    go (Match _ matchLen repl : rest) !acc = go rest (acc - matchLen + Aho.lengthUtf16 repl)

-- | Given a list of matches sorted on start position, remove matches that start
-- within an earlier match.
removeOverlap :: [Match] -> [Match]
removeOverlap matches = case matches of
  [] -> []
  m:[] -> m:[]
  (m0@(Match pos0 len0 _) : m1@(Match pos1 _ _) : ms) ->
    if pos1 >= pos0 + len0
      then m0 : removeOverlap (m1:ms)
      else removeOverlap (m0:ms)

-- | When we iterate through all matches, keep track only of the matches with
-- the highest priority: those are the ones that we will replace first. If we
-- find multiple matches with that priority, remember all of them. If we find a
-- match with lower priority, ignore it, because we already have a more
-- important match. Also, if the priority is `threshold` or higher, ignore the
-- match, so we can exclude matches if we already did a round of replacements
-- for that priority. This way we don't have to build a new automaton after
-- every round of replacements.
{-# INLINE prependMatch #-}
prependMatch :: Priority -> (Priority, [Match]) -> Aho.Match Payload -> Aho.Next (Priority, [Match])
prependMatch !threshold (!pBest, !matches) (Aho.Match pos (Payload pMatch len replacement))
  | pMatch < threshold && pMatch >  pBest = Aho.Step (pMatch, [Match (pos - len) len replacement])
  | pMatch < threshold && pMatch == pBest = Aho.Step (pMatch, (Match (pos - len) len replacement) : matches)
  | otherwise = Aho.Step (pBest, matches)

run :: Replacer -> Text -> Text
run replacer = fromJust . runWithLimit replacer maxBound

{-# NOINLINE runWithLimit #-}
runWithLimit :: Replacer -> CodeUnitIndex -> Text -> Maybe Text
runWithLimit (Replacer case_ searcher) maxLength = go initialThreshold
  where
    !automaton = Searcher.automaton searcher

    -- Priorities are 0 or lower, so an initial threshold of 1 keeps all
    -- matches.
    !initialThreshold = 1

    -- Needle priorities go from 0 for the highest priority to (-numNeedles + 1)
    -- for the lowest priority. That means that if we find a match with
    -- minPriority, we don't need to do another pass afterwards, because there
    -- are no remaining needles.
    !minPriority = 1 - Searcher.numNeedles searcher

    go :: Priority -> Text -> Maybe Text
    go !threshold haystack =
      let
        seed = (minBound :: Priority, [])
        matchesWithPriority = case case_ of
          CaseSensitive -> Aho.runText seed (prependMatch threshold) automaton haystack
          IgnoreCase -> Aho.runLower seed (prependMatch threshold) automaton haystack
      in
        case matchesWithPriority of
          -- No match at the given threshold, there is nothing left to do.
          -- Return the input string unmodified.
          (_, []) -> Just haystack
          -- We found matches at priority p. Remove overlapping matches, then
          -- apply all replacements. Next, we need to go again, this time
          -- considering only needles with a lower priority than p. As an
          -- optimization (which matters mainly for the single needle case),
          -- if we find a match at the lowest priority, we don't need another
          -- pass. Note that if in `rawMatches` we find only matches of priority
          -- p > minPriority, then we do still need another pass, because the
          -- replacements could create new matches.
          (p, matches)
            | replacementLength matches haystack > maxLength -> Nothing
            | p == minPriority -> Just $ replace (removeOverlap $ sort matches) haystack
            | otherwise -> go p $ replace (removeOverlap $ sort matches) haystack
