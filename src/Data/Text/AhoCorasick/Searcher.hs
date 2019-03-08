-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

-- See AhoCorasick.Automaton for more info about these GHC flags.
-- TL;DR: They make things faster, and we need the flags here because the
-- functions from that module may be inlined into this module.
{-# OPTIONS_GHC -fllvm -O2 -optlo=-O3 -optlo=-tailcallelim -fno-ignore-asserts #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Text.AhoCorasick.Searcher
  ( Searcher
  , build
  , buildWithValues
  , needles
  , numNeedles
  , automaton
  , containsAny
  , containsAnyIgnoreCase
  )
  where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable (hashWithSalt), Hashed, hashed, unhashed)
import Data.Semigroup (Semigroup, (<>))
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Text.AhoCorasick.Automaton as Aho

-- | A set of needles with associated values, and an Aho-Corasick automaton to
-- efficiently find those needles.
--
-- INVARIANT: searcherAutomaton = Aho.build . searcherNeedles
-- To enforce this invariant, the fields are not exposed from this module.
-- There is a separate constructor function.
--
-- The purpose of this wrapper is to have a type that is Hashable and Eq, so we
-- can derive those for types that embed the searcher, whithout requiring the
-- automaton itself to be Hashable or Eq, which would be both wasteful and
-- tedious. Because the automaton is fully determined by the needles and
-- associated values, it is sufficient to implement Eq and Hashable in terms of
-- the needles only.
--
-- We also use Hashed to cache the hash of the needles.
data Searcher v = Searcher
  { searcherNeedles :: Hashed [(Text, v)]
  , searcherNumNeedles :: Int
  , searcherAutomaton :: Aho.AcMachine v
  } deriving (Generic)

instance Show (Searcher v) where
  show _ = "Searcher _ _ _"

instance Hashable v => Hashable (Searcher v) where
  hashWithSalt salt searcher = hashWithSalt salt $ searcherNeedles searcher

instance Eq v => Eq (Searcher v) where
  -- Since we store the length of the needle list anyway,
  -- we can use it to early out if there is a length mismatch.
  Searcher xs nx _ == Searcher ys ny _ = (nx, xs) == (ny, ys)

instance NFData v => NFData (Searcher v)

-- NOTE: Although we could implement Semigroup for every v by just concatenating
-- needle lists, we don't, because this might lead to unexpected results. For
-- example, if v is (Int, a) where the Int is a priority, combining two
-- searchers might want to discard priorities, concatenate the needle lists, and
-- reassign priorities, rather than concatenating the needle lists as-is and
-- possibly having duplicate priorities in the resulting searcher.
instance Semigroup (Searcher ()) where
  x <> y = buildWithValues (needles x <> needles y)

build :: [Text] -> Searcher ()
build = buildWithValues . fmap (\x -> (x, ()))

buildWithValues :: Hashable v => [(Text, v)] -> Searcher v
buildWithValues ns =
  let
    unpack (text, value) = (Aho.unpackUtf16 text, value)
  in
    Searcher (hashed ns) (length ns) $ Aho.build $ fmap unpack ns

needles :: Searcher v -> [(Text, v)]
needles = unhashed . searcherNeedles

numNeedles :: Searcher v -> Int
numNeedles = searcherNumNeedles

automaton :: Searcher v -> Aho.AcMachine v
automaton = searcherAutomaton

-- | Return whether the haystack contains any of the needles.
-- Is case sensitive.
-- This function is marked noinline as an inlining boundary. Aho.runText is
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
    f _acc _match = Aho.Done True
  in
    Aho.runText False f (automaton searcher) text

-- | Return whether the haystack contains any of the needles.
-- Is case insensitive. The needles in the searcher should be lowercase.
{-# NOINLINE containsAnyIgnoreCase #-}
containsAnyIgnoreCase :: Searcher () -> Text -> Bool
containsAnyIgnoreCase !searcher !text =
  let
    -- On the first match, return True immediately.
    f _acc _match = Aho.Done True
  in
    Aho.runLower False f (automaton searcher) text
