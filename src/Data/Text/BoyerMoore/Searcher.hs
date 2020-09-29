-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

-- See Data.Text.AhoCorasick.Automaton for more info about these GHC flags.
-- TL;DR: They make things faster, and we need the flags here because the
-- functions from that module may be inlined into this module.
{-# OPTIONS_GHC -fllvm -O2 -optlo=-O3 -optlo=-tailcallelim -fignore-asserts #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Text.BoyerMoore.Searcher
  ( Searcher
  , build
  , buildWithValues
  , needles
  , numNeedles
  , automata
  , caseSensitivity
  , containsAny
  , setSearcherCaseSensitivity
  )
  where


import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Hashable (Hashable (hashWithSalt), Hashed, hashed, unhashed)
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Text.BoyerMoore.Automaton (Automaton, CaseSensitivity (..))

import qualified Data.Text.BoyerMoore.Automaton as BoyerMoore

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
  { searcherCaseSensitive :: CaseSensitivity
  , searcherNeedles :: Hashed [(Text, v)]
  , searcherNumNeedles :: Int
  , searcherAutomata :: [(Automaton, v)]
  } deriving (Generic)

instance Show (Searcher v) where
  show _ = "Searcher _ _ _"

instance Hashable v => Hashable (Searcher v) where
  hashWithSalt salt searcher = hashWithSalt salt $ searcherNeedles searcher
  {-# INLINE hashWithSalt #-}

instance Eq v => Eq (Searcher v) where
  Searcher cx xs nx _ == Searcher cy ys ny _ = (cx, nx, xs) == (cy, ny, ys)
  {-# INLINE (==) #-}

instance NFData v => NFData (Searcher v)

-- | Builds the Searcher for a list of needles
-- The caller is responsible that the needles are lower case in case the IgnoreCase
-- is used for case sensitivity
build :: CaseSensitivity -> [Text] -> Searcher ()
{-# INLINABLE build #-}
build case_ = buildWithValues case_ . flip zip (repeat ())

-- | The caller is responsible that the needles are lower case in case the IgnoreCase
-- is used for case sensitivity
buildWithValues :: Hashable v => CaseSensitivity -> [(Text, v)] -> Searcher v
{-# INLINABLE buildWithValues #-}
buildWithValues case_ ns =
  Searcher case_ (hashed ns) (length ns) $ map (first BoyerMoore.buildAutomaton) ns

needles :: Searcher v -> [(Text, v)]
needles = unhashed . searcherNeedles

automata :: Searcher v -> [(Automaton, v)]
automata = searcherAutomata

numNeedles :: Searcher v -> Int
numNeedles = searcherNumNeedles

caseSensitivity :: Searcher v -> CaseSensitivity
caseSensitivity = searcherCaseSensitive

-- | Updates the case sensitivity of the searcher. Does not change the
-- capitilization of the needles. The caller should be certain that if IgnoreCase
-- is passed, the needles are already lower case.
setSearcherCaseSensitivity :: CaseSensitivity -> Searcher v -> Searcher v
setSearcherCaseSensitivity case_ searcher = searcher{
    searcherCaseSensitive = case_
  }


-- | Return whether the haystack contains any of the needles.
-- Case sensitivity depends on the properties of the searcher
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
    f _acc _match = BoyerMoore.Done True
  in
    case caseSensitivity searcher of
      CaseSensitive ->
        any (\(automaton, ()) -> BoyerMoore.runText False f automaton text) (automata searcher)
      IgnoreCase ->
        any (\(automaton, ()) -> BoyerMoore.runLower False f automaton text) (automata searcher)
