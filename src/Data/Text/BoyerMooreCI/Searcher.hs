-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Text.BoyerMooreCI.Searcher
    ( Searcher
    , automata
    , build
    , buildNeedleIdSearcher
    , buildWithValues
    , containsAll
    , containsAny
    , needles
    , numNeedles
    ) where


import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Hashable (Hashable (hashWithSalt), Hashed, hashed, unhashed)
import GHC.Generics (Generic)

import Data.Text.Utf8 (Text)
import Data.Text.BoyerMooreCI.Automaton (Automaton)

import qualified Data.Text.BoyerMooreCI.Automaton as BoyerMoore


-- | A set of needles with associated values, and Boyer-Moore automata to
-- efficiently find those needles.
--
-- INVARIANT: searcherAutomaton = BoyerMoore.buildAutomaton . searcherNeedles
-- To enforce this invariant, the fields are not exposed from this module.
-- There is a separate constructor function.
--
-- The purpose of this wrapper is to have a type that is Hashable and Eq, so we
-- can derive those for the types that embed the searcher, whithout
-- requiring the automaton itself to be Hashable or Eq, which would be both
-- wasteful and tedious. Because the automaton is fully determined by the
-- needles and associated values, it is sufficient to implement Eq and Hashable
-- in terms of the needles only.
--
-- We also use Hashed to cache the hash of the needles.
data Searcher v = Searcher
  { searcherNeedles :: Hashed [(Text, v)]
  , searcherNumNeedles :: Int
  , searcherAutomata :: [(Automaton, v)]
  } deriving (Generic)

instance Show (Searcher v) where
  show _ = "Searcher _ _ _"

instance Hashable v => Hashable (Searcher v) where
  hashWithSalt salt searcher = hashWithSalt salt $ searcherNeedles searcher
  {-# INLINE hashWithSalt #-}

instance Eq v => Eq (Searcher v) where
  Searcher xs nx _ == Searcher ys ny _ = (xs, nx) == (ys, ny)
  {-# INLINE (==) #-}

instance NFData v => NFData (Searcher v)

-- | Builds the Searcher for a list of needles without values.
-- This is useful for just checking whether the haystack contains the needles.
build :: [Text] -> Searcher ()
{-# INLINABLE build #-}
build = buildWithValues . flip zip (repeat ())

-- | Builds the Searcher for a list of needles.
buildWithValues :: Hashable v => [(Text, v)] -> Searcher v
{-# INLINABLE buildWithValues #-}
buildWithValues ns =
  Searcher (hashed ns) (length ns) $ map (first BoyerMoore.buildAutomaton) ns

needles :: Searcher v -> [(Text, v)]
needles = unhashed . searcherNeedles

automata :: Searcher v -> [(Automaton, v)]
automata = searcherAutomata

numNeedles :: Searcher v -> Int
numNeedles = searcherNumNeedles

-- | Return whether the haystack contains any of the needles.
-- This function is marked noinline as an inlining boundary. BoyerMoore.runText is
-- marked inline, so this function will be optimized to report only whether
-- there is a match, and not construct a list of matches. We don't want this
-- function be inline, to make sure that the conditions of the caller don't
-- affect how this function is optimized. There is little to gain from
-- additional inlining. The pragma is not an optimization in itself, rather it
-- is a defence against fragile optimizer decisions.
{-# NOINLINE containsAny #-}
containsAny :: Searcher () -> Text -> Bool
containsAny !searcher !text =
  let
    -- On the first match, return True immediately.
    f _acc _matchStart _matchEnd = BoyerMoore.Done True
  in
    any (\(automaton, ()) -> BoyerMoore.runText False f automaton text) (automata searcher)
-- | Build a 'Searcher' that returns the needle's index in the needle list when it matches.

buildNeedleIdSearcher :: [Text] -> Searcher Int
buildNeedleIdSearcher !ns =
  buildWithValues $ zip ns [0..]

-- | Like 'containsAny', but checks whether all needles match instead.
-- Use 'buildNeedleIdSearcher' to get an appropriate 'Searcher'.
{-# NOINLINE containsAll #-}
containsAll :: Searcher Int -> Text -> Bool
containsAll !searcher !text =
  let
    -- On the first match, return True immediately.
    f _acc _matchStart _matchEnd = BoyerMoore.Done True
  in
    all (\(automaton, _) -> BoyerMoore.runText False f automaton text) (automata searcher)
