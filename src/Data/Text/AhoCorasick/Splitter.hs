-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

-- | Splitting strings using Aho–Corasick.
module Data.Text.AhoCorasick.Splitter
    ( Splitter
    , automaton
    , build
    , separator
    , split
    , splitIgnoreCase
    , splitReverse
    , splitReverseIgnoreCase
    ) where

import Control.DeepSeq (NFData (..))
import Data.Function (on)
import Data.Hashable (Hashable (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text.Utf8 (Text)

#if defined(HAS_AESON)
import qualified Data.Aeson as AE
#endif

import qualified Data.List.NonEmpty as NonEmpty

import Data.Text.AhoCorasick.Automaton (AcMachine)

import qualified Data.Text.Utf8 as Utf8
import qualified Data.Text.AhoCorasick.Automaton as Aho

--------------------------------------------------------------------------------
-- Splitter

-- | Build a splitter once, then use it many times!
data Splitter =
  Splitter
    { splitterAutomaton :: AcMachine () -- INVARIANT: Exactly one needle.
    , splitterSeparator :: Text         -- INVARIANT: Equivalent to needle.
    }

#if defined(HAS_AESON)
instance AE.ToJSON Splitter where
  toJSON = AE.toJSON . separator

instance AE.FromJSON Splitter where
  parseJSON v = build <$> AE.parseJSON v
#endif

-- | Construct a splitter with a single separator.
{-# INLINE build #-}
build :: Text -> Splitter
build sep =
  let !auto = Aho.build [(sep, ())] in
  Splitter auto sep

-- | Get the automaton that would be used for finding separators.
{-# INLINE automaton #-}
automaton :: Splitter -> AcMachine ()
automaton = splitterAutomaton

-- | What is the separator we are splitting on?
{-# INLINE separator #-}
separator :: Splitter -> Text
separator = splitterSeparator

-- | Split the given string into strings separated by the separator.
--
-- If the order of the results is not important, use the faster function
-- 'splitReverse' instead.
{-# INLINE split #-}
split :: Splitter -> Text -> NonEmpty Text
split = (NonEmpty.reverse .) . splitReverse

-- | Split the given string into strings separated by the separator.
--
-- If the order of the results is not important, use the faster function
-- 'splitReverseIgnoreCase' instead.
--
-- The separator is matched case-insensitively, but the splitter must have been
-- constructed with a lowercase needle.
{-# INLINE splitIgnoreCase #-}
splitIgnoreCase :: Splitter -> Text -> NonEmpty Text
splitIgnoreCase = (NonEmpty.reverse .) . splitReverseIgnoreCase

-- | Like 'split', but return the substrings in reverse order.
{-# INLINE splitReverse #-}
splitReverse :: Splitter -> Text -> NonEmpty Text
splitReverse s t =
  finalizeAccum $
    Aho.runText
      (zeroAccum (separator s) t)
      stepAccum
      (automaton s)
      t

-- | Like 'splitIgnoreCase', but return the substrings in reverse order.
{-# INLINE splitReverseIgnoreCase #-}
splitReverseIgnoreCase :: Splitter -> Text -> NonEmpty Text
splitReverseIgnoreCase s t =
  finalizeAccum $
    Aho.runLower
      (zeroAccum (separator s) t)
      stepAccum
      (automaton s)
      t

--------------------------------------------------------------------------------
-- Fold

-- | The accumulator is used as state when processing the matches from left to
-- right. While the matches are fed to us ordered by end offset, all matches
-- have the same length because there is only one needle.
data Accum =
  Accum
    { _accumSepLen   :: !Aho.CodeUnitIndex -- ^ Length of separator.
    , _accumHaystack :: !Text              -- ^ Haystack to slice off of.
    , accumResult    :: ![Text]            -- ^ Match-separated strings.
    , accumPrevEnd   :: !Aho.CodeUnitIndex -- ^ Offset at end of last match.
    }

-- | Finalizing the accumulator does more than just 'accumResult', hence this
-- is a separate function.
{-# INLINE finalizeAccum #-}
finalizeAccum :: Accum -> NonEmpty Text
finalizeAccum (Accum _ hay res prevEnd) =
  -- Once we have processed all the matches, there is still the substring after
  -- the final match. This substring is always included in the result, even
  -- when there were no matches. Hence we can return a non-empty list.
  let !str = Utf8.unsafeSliceUtf8 prevEnd (Utf8.lengthUtf8 hay - prevEnd) hay in
  str :| res

-- | The initial accumulator begins at the begin of the haystack.
{-# INLINE zeroAccum #-}
zeroAccum :: Text -> Text -> Accum
zeroAccum sep hay = Accum (Utf8.lengthUtf8 sep) hay [] 0

-- | Step the accumulator using the next match. Overlapping matches will be
-- ignored. Overlapping matches may occur when the separator has a non-empty
-- prefix that is also a suffix.
{-# INLINE stepAccum #-}
stepAccum :: Accum -> Aho.Match v -> Aho.Next Accum
stepAccum acc@(Accum sepLen hay res prevEnd) (Aho.Match sepEnd _)

  -- When the match begins before the current offset, it overlaps a match that
  -- we processed before, and so we ignore it.
  | sepEnd - sepLen < prevEnd =
      Aho.Step acc

  -- The match is behind the current offset, so we slice the haystack until the
  -- begin of the match and include that as a result.
  | otherwise =
      let !str = Utf8.unsafeSliceUtf8 prevEnd (sepEnd - sepLen - prevEnd) hay in
      Aho.Step acc { accumResult = str : res, accumPrevEnd = sepEnd }

--------------------------------------------------------------------------------
-- Instances

instance Eq Splitter where
  {-# INLINE (==) #-}
  (==) = (==) `on` separator

instance Ord Splitter where
  {-# INLINE compare #-}
  compare = compare `on` separator

instance Hashable Splitter where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt searcher =
    salt `hashWithSalt` separator searcher

instance NFData Splitter where
  {-# INLINE rnf #-}
  rnf (Splitter searcher sepLength) =
    rnf searcher `seq`
    rnf sepLength

instance Show Splitter where
  showsPrec p splitter =
    showParen (p > 10) $
      showString "build " .
        showsPrec 11 (separator splitter)
