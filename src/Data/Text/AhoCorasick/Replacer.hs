-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Implements sequential string replacements based on the Aho-Corasick algorithm.
module Data.Text.AhoCorasick.Replacer
    ( -- * State machine
      Needle
    , Payload (..)
    , Replacement
    , Replacer (..)
    , replacerCaseSensitivity
    , build
    , compose
    , mapReplacement
    , run
    , runWithLimit
    , setCaseSensitivity
    ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.List (sort)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)

#if defined(HAS_AESON)
import qualified Data.Aeson as AE
#endif

import Data.Text.AhoCorasick.Searcher (Searcher)
import Data.Text.CaseSensitivity (CaseSensitivity (..))
import Data.Text.Utf8 (CodeUnitIndex (..), Text)

import qualified Data.Text as Text
import qualified Data.Text.AhoCorasick.Automaton as Aho
import qualified Data.Text.AhoCorasick.Searcher as Searcher
import qualified Data.Text.Utf8 as Utf8

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
  , needleLengthBytes       :: {-# UNPACK #-} !CodeUnitIndex
    -- ^ Number of bytes is used for case sensitive matching
  , needleLengthCodePoints  :: {-# UNPACK #-} !Int
    -- ^ For case insensitive matches, the byte length does not necessarily match the needle byte
    -- length. Due to our simple case folding the number of codepoints _does_ match, so we put that
    -- in the payload. It's less efficient because we have to scan backwards through the text to
    -- obtain the length of a match.

  , needleReplacement :: !Replacement
  }
#if defined(HAS_AESON)
  deriving (Eq, Generic, Hashable, NFData, Show, AE.FromJSON, AE.ToJSON)
#else
  deriving (Eq, Generic, Hashable, NFData, Show)
#endif

-- | A state machine used for efficient replacements with many different needles.
data Replacer = Replacer
  { replacerSearcher :: Searcher Payload
  }
  deriving stock (Show, Eq, Generic)
#if defined(HAS_AESON)
  deriving (Hashable, NFData, AE.FromJSON, AE.ToJSON)
#else
  deriving (Hashable, NFData)
#endif

-- | Build an Aho-Corasick automaton that can be used for performing fast
-- sequential replaces.
--
-- Case-insensitive matching performs per-letter language-agnostic lower-casing.
-- Therefore, it will work in most cases, but not in languages where lower-casing
-- depends on the context of the character in question.
--
-- We need to revisit this algorithm when we want to implement full Unicode
-- support.
build :: CaseSensitivity -> [(Needle, Replacement)] -> Replacer
build caseSensitivity replaces = Replacer searcher
  where
    searcher = Searcher.buildWithValues caseSensitivity $ zipWith mapNeedle [0..] replaces
    mapNeedle i (needle, replacement) =
      -- Note that we negate i: earlier needles have a higher priority. We
      -- could avoid it and define larger integers to be lower priority, but
      -- that made the terminology in this module very confusing.
      let needle' = case Searcher.caseSensitivity searcher of
                      CaseSensitive -> needle
                      IgnoreCase -> Utf8.lowerUtf8 needle
          -- Payload includes byte and code point lengths, so can still be used if we change case
          -- sensitivity later.
          payload = Payload
            { needlePriority = (-i)
            , needleLengthBytes = Utf8.lengthUtf8 needle
            , needleLengthCodePoints = Text.length needle
            , needleReplacement = replacement
            }
      in (needle', payload)

-- | Return the composition `replacer2` after `replacer1`, if they have the same
-- case sensitivity. If the case sensitivity differs, Nothing is returned.
compose :: Replacer -> Replacer -> Maybe Replacer
compose (Replacer searcher1) (Replacer searcher2)
  | Searcher.caseSensitivity searcher1 /= Searcher.caseSensitivity searcher2 = Nothing
  | otherwise =
      let
        -- Replace the priorities of the second machine, so they all come after
        -- the first.
        renumber i (needle, Payload _ lenb lenc replacement) = (needle, Payload (-i) lenb lenc replacement)
        needles1 = Searcher.needles searcher1
        needles2 = Searcher.needles searcher2
        cs = Searcher.caseSensitivity searcher1
        searcher = Searcher.buildWithValues cs $ zipWith renumber [0..] (needles1 ++ needles2)
      in
        Just $ Replacer searcher

-- | Modify the replacement of a replacer. It doesn't modify the needles.
mapReplacement :: (Replacement -> Replacement) -> Replacer -> Replacer
mapReplacement f replacer = replacer{
  replacerSearcher = Searcher.mapSearcher
    (\p -> p {needleReplacement = f (needleReplacement p)})
    (replacerSearcher replacer)
}


replacerCaseSensitivity :: Replacer -> CaseSensitivity
replacerCaseSensitivity (Replacer searcher) = Searcher.caseSensitivity searcher


-- | Updates the case sensitivity of the replacer. Does not change the
-- capitilization of the needles. The caller should be certain that if IgnoreCase
-- is passed, the needles are already lower case.
setCaseSensitivity :: CaseSensitivity -> Replacer -> Replacer
setCaseSensitivity case_ (Replacer searcher) =
  Replacer (Searcher.setCaseSensitivity case_ searcher)


-- A match collected while running replacements. It is isomorphic to the Match
-- reported by the automaton, but the data is arranged in a more useful way:
-- as the start index and length of the match, and the replacement.
data Match = Match !CodeUnitIndex !CodeUnitIndex !Text deriving (Eq, Ord, Show, Generic)

-- | Apply replacements of all matches. Assumes that the matches are ordered by
-- match position, and that no matches overlap.
replace :: [Match] -> Text -> Text
replace matches haystack = Utf8.concat $ go 0 matches haystack
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
        (prefix, suffix) = Utf8.unsafeCutUtf8 (pos - offset) len remainder
      in
        prefix : replacement : go (pos + len) ms suffix

-- | Compute the length of the string resulting from applying the replacements.
replacementLength :: [Match] -> Text -> CodeUnitIndex
replacementLength matches initial  = go matches (Utf8.lengthUtf8 initial)
  where
    go [] !acc = acc
    go (Match _ matchLen repl : rest) !acc = go rest (acc - matchLen + Utf8.lengthUtf8 repl)

-- | Given a list of matches sorted on start position, remove matches that start
-- within an earlier match.
removeOverlap :: [Match] -> [Match]
removeOverlap matches = case matches of
  [] -> []
  [m] -> [m]
  (m0@(Match pos0 len0 _) : m1@(Match pos1 _ _) : ms) ->
    if pos1 >= pos0 + len0
      then m0 : removeOverlap (m1:ms)
      else removeOverlap (m0:ms)

run :: Replacer -> Text -> Text
run replacer = fromJust . runWithLimit replacer maxBound

{-# NOINLINE runWithLimit #-}
runWithLimit :: Replacer -> CodeUnitIndex -> Text -> Maybe Text
runWithLimit (Replacer searcher) maxLength = go initialThreshold
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
        matchesWithPriority = case Searcher.caseSensitivity searcher of
          CaseSensitive -> Aho.runText seed (prependMatch threshold haystack) automaton haystack
          IgnoreCase -> Aho.runLower seed (prependMatch threshold haystack) automaton haystack
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

    -- When we iterate through all matches, keep track only of the matches with
    -- the highest priority: those are the ones that we will replace first. If we
    -- find multiple matches with that priority, remember all of them. If we find a
    -- match with lower priority, ignore it, because we already have a more
    -- important match. Also, if the priority is `threshold` or higher, ignore the
    -- match, so we can exclude matches if we already did a round of replacements
    -- for that priority. This way we don't have to build a new automaton after
    -- every round of replacements.
    prependMatch
      :: Priority -> Text -> (Priority, [Match]) -> Aho.Match Payload -> Aho.Next (Priority, [Match])
    {-# INLINE prependMatch #-}
    prependMatch !threshold haystack (!pBest, !matches) (Aho.Match pos (Payload pMatch lenb lenc replacement))
      | pMatch < threshold && pMatch >  pBest =
          Aho.Step (pMatch, [makeMatch haystack pos lenb lenc replacement])
      | pMatch < threshold && pMatch == pBest =
          Aho.Step (pMatch, makeMatch haystack pos lenb lenc replacement : matches)
      | otherwise = Aho.Step (pBest, matches)

    -- Pos is the code unit index past the last code unit of the match, we have
    -- to find the first code unit.
    makeMatch :: Text -> CodeUnitIndex -> CodeUnitIndex -> Int -> Replacement -> Match
    {-# INLINE makeMatch #-}
    makeMatch = case Searcher.caseSensitivity searcher of
      -- Case sensitive: length is interpreted as number of bytes
      CaseSensitive -> \_ pos lenb _ replacement ->
        Match (pos - lenb) lenb replacement
      -- Case insensitive: length is interpreted as number of characters
      IgnoreCase -> \haystack pos _ lenc replacement ->
        -- We start at the last byte of the match, and look backwards.
        let start = Utf8.skipCodePointsBackwards haystack (pos-1) (lenc-1) in
        Match start (pos - start) replacement
