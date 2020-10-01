-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.AhoCorasick.Searcher
  ( Searcher
  , build
  , buildWithValues
  , needles
  , numNeedles
  , automaton
  , caseSensitivity
  , containsAny
  , setSearcherCaseSensitivity
  )
  where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable (hashWithSalt), Hashed, hashed, unhashed)
import Data.Semigroup (Semigroup, (<>))
import Data.Text (Text)
import GHC.Generics (Generic)

#if defined(HAS_AESON)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as AE
#endif

import Data.Text.AhoCorasick.Automaton (CaseSensitivity (..))

import qualified Data.Text.AhoCorasick.Automaton as Aho
import qualified Data.Text.Utf16 as Utf16

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
  { searcherCaseSensitive :: CaseSensitivity
  , searcherNeedles :: Hashed [(Text, v)]
  , searcherNumNeedles :: Int
  , searcherAutomaton :: Aho.AcMachine v
  } deriving (Generic)

#if defined(HAS_AESON)
instance AE.ToJSON v => AE.ToJSON (Searcher v) where
  toJSON s = AE.object
    [ "needles" .= needles s
    , "caseSensitivity" .= caseSensitivity s
    ]

instance (Hashable v, AE.FromJSON v) => AE.FromJSON (Searcher v) where
  parseJSON = AE.withObject "Searcher" $ \o -> buildWithValues <$> o .: "caseSensitivity" <*> o .: "needles"
#endif

instance Show (Searcher v) where
  show _ = "Searcher _ _ _"

instance Hashable v => Hashable (Searcher v) where
  hashWithSalt salt searcher = hashWithSalt salt $ searcherNeedles searcher
  {-# INLINE hashWithSalt #-}

instance Eq v => Eq (Searcher v) where
  -- Since we store the length of the needle list anyway,
  -- we can use it to early out if there is a length mismatch.
  Searcher cx xs nx _ == Searcher cy ys ny _ = (nx, xs, cx) == (ny, ys, cy)
  {-# INLINE (==) #-}

instance NFData v => NFData (Searcher v)

-- NOTE: Although we could implement Semigroup for every v by just concatenating
-- needle lists, we don't, because this might lead to unexpected results. For
-- example, if v is (Int, a) where the Int is a priority, combining two
-- searchers might want to discard priorities, concatenate the needle lists, and
-- reassign priorities, rather than concatenating the needle lists as-is and
-- possibly having duplicate priorities in the resulting searcher.
instance Semigroup (Searcher ()) where
  x <> y
    | caseSensitivity x == caseSensitivity y
      = buildWithValues (searcherCaseSensitive x) (needles x <> needles y)
    | otherwise = error "Combining searchers of different case sensitivity"
  {-# INLINE (<>) #-}

-- | Builds the Searcher for a list of needles
-- The caller is responsible that the needles are lower case in case the IgnoreCase
-- is used for case sensitivity
build :: CaseSensitivity -> [Text] -> Searcher ()
build case_ = buildWithValues case_ . fmap (\x -> (x, ()))

-- | The caller is responsible that the needles are lower case in case the IgnoreCase
-- is used for case sensitivity
buildWithValues :: Hashable v => CaseSensitivity -> [(Text, v)] -> Searcher v
{-# INLINABLE buildWithValues #-}
buildWithValues case_ ns =
  let
    unpack (text, value) = (Utf16.unpackUtf16 text, value)
  in
    Searcher case_ (hashed ns) (length ns) $ Aho.build $ fmap unpack ns

needles :: Searcher v -> [(Text, v)]
needles = unhashed . searcherNeedles

numNeedles :: Searcher v -> Int
numNeedles = searcherNumNeedles

automaton :: Searcher v -> Aho.AcMachine v
automaton = searcherAutomaton

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
  in case caseSensitivity searcher of
    CaseSensitive  -> Aho.runText False f (automaton searcher) text
    IgnoreCase      -> Aho.runLower False f (automaton searcher) text
