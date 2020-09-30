-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

-- Compile this module with LLVM, rather than with the default code generator.
-- LLVM produces about 20% faster code.
{-# OPTIONS_GHC -fllvm -O2 -optlo=-O3 -optlo=-tailcallelim -fignore-asserts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An efficient implementation of the Boyer-Moore string search algorithm.
-- http://www-igm.univ-mlv.fr/~lecroq/string/node14.html#SECTION00140
-- https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string-search_algorithm
--
-- This module contains a almost 1:1 translation from the C example code in the
-- wikipedia article.
--
-- The algorithm here can be potentially improved by including the Galil rule
-- (https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string-search_algorithm#The_Galil_rule)
module Data.Text.BoyerMoore.Automaton
  (
    Automaton
  , CaseSensitivity (..)
  , buildAutomaton
  , runText
  , runLower
  , patternLength
  , patternText

  , CodeUnitIndex (..)
  , Next (..)
  )
  where

import Prelude hiding (length)

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.ST (runST)
import Data.Hashable (Hashable (..), Hashed, hashed, unhashed)
import Data.Text.Internal (Text (..))
import GHC.Generics (Generic)

#if defined(HAS_AESON)
import qualified Data.Aeson as AE
#endif
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Unboxed.Mutable as UMVector

import Data.Text.AhoCorasick.Automaton (CaseSensitivity (..), Next (..))
import Data.Text.Utf16 (CodeUnit, CodeUnitIndex (..), lengthUtf16, lowerCodeUnit, lengthUtf16, unsafeIndexUtf16)

-- | A Boyer-Moore automaton is based on lookup-tables that allow skipping through the haystack.
-- This allows for sub-linear matching in some cases, as we do not have to look at every input
-- character.
--
-- NOTE: Unlike the AcMachine, a Boyer-Moore automaton only returns non-overlapping matches.
-- This means that a Boyer-Moore automaton is not a 100% drop-in replacement for Aho-Corasick.
--
-- Returning overlapping matches would degrade the performance to /O(nm)/ in pathological cases like
-- finding @aaaa@ in @aaaaa....aaaaaa@ as for each match it would scan back the whole /m/ characters
-- of the pattern.
data Automaton = Automaton
  { automatonPattern :: Hashed Text
  , automatonSuffixTable :: SuffixTable
  , automatonBadCharTable :: BadCharTable
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

instance Hashable Automaton where
  hashWithSalt salt (Automaton pattern _ _) = hashWithSalt salt pattern

instance Eq Automaton where
  (Automaton pat1 _ _) == (Automaton pat2 _ _) = pat1 == pat2

#if defined(HAS_AESON)
instance AE.FromJSON Automaton where
  parseJSON v = buildAutomaton <$> AE.parseJSON v

instance AE.ToJSON Automaton where
  toJSON = AE.toJSON . unhashed . automatonPattern
#endif

buildAutomaton :: Text -> Automaton
buildAutomaton pattern = Automaton (hashed pattern) (buildSuffixTable pattern) (buildBadCharTable pattern)

runWithCase
  :: forall a
  .  CaseSensitivity
  -> a
  -> (a -> CodeUnitIndex -> Next a)
  -> Automaton
  -> Text
  -> a
{-# INLINE runWithCase #-}
runWithCase caseSensitivity seed f automaton text
  | patLen == 0 = seed
  | otherwise = go seed (patLen - 1)
  where
    Automaton patternHashed suffixTable badCharTable = automaton
    pattern = unhashed patternHashed
    patLen = lengthUtf16 pattern
    stringLen = lengthUtf16 text

    inputCasedAt = case caseSensitivity of
      CaseSensitive -> indexCodePoint text
      IgnoreCase -> lowerCodeUnit . indexCodePoint text

    go result haystackIndex
      | haystackIndex < stringLen = matchLoop result haystackIndex (patLen - 1)
      | otherwise = result
    {-# INLINE go #-}

    -- Compare the needle back-to-front with the haystack
    matchLoop result haystackIndex needleIndex
      | needleIndex >= 0 && inputCasedAt haystackIndex == indexCodePoint pattern needleIndex =
        -- Characters match, try the pair before
        matchLoop result (haystackIndex - 1) (needleIndex - 1)
      -- We found a match (all needle characters matched)
      | needleIndex < 0 =
        case f result (haystackIndex + 1) of
          Done final -> final
          -- `haystackIndex` now points to the character just before the match starts
          -- Adding `patLen` once points to the last character of the match,
          -- Adding `patLen` once more points to the earliest character where
          -- we can find a non-overlapping match.
          Step intermediate -> go intermediate (haystackIndex + 2 * patLen)
      -- We know it's not a match, the characters differ at the current position
      | otherwise =
        let
          -- The bad character table tells us how far we can advance to the right so that the
          -- character at the current position in the input string, where matching failed,
          -- is lined up with it's rightmost occurrence in the needle.
          -- Note: we could end up left of were we started, essentially never making progress,
          -- if we were to use this rule alone.
          badCharSkip = badCharLookup badCharTable (inputCasedAt haystackIndex)
          suffixSkip = suffixLookup suffixTable needleIndex
          skip = max badCharSkip suffixSkip
        in
          go result (haystackIndex + skip)

-- | Finds all matches in the text, calling the match callback with the *first*
-- matched character of each match of the pattern.
--
-- NOTE: This is unlike Aho-Corasick, which reports the index of the character
-- right after a match.
--
-- NOTE: To get full advantage of inlining this function, you probably want to
-- compile the compiling module with -fllvm and the same optimization flags as
-- this module.
{-# INLINE runText #-}
runText :: forall a. a -> (a -> CodeUnitIndex -> Next a) -> Automaton -> Text -> a
runText = runWithCase CaseSensitive

-- | Finds all matches in the lowercased text. This function lowercases the text
-- on the fly to avoid allocating a second lowercased text array. Lowercasing is
-- applied to individual code units, so the indexes into the lowercased text can
-- be used to index into the original text. It is still the responsibility of
-- the caller to lowercase the needles. Needles that contain uppercase code
-- points will not match.
--
-- NOTE: To get full advantage of inlining this function, you probably want to
-- compile the compiling module with -fllvm and the same optimization flags as
-- this module.
{-# INLINE runLower #-}
runLower :: forall a. a -> (a -> CodeUnitIndex -> Next a) -> Automaton -> Text -> a
runLower = runWithCase IgnoreCase

-- | Length of the matched pattern measured in Utf16 code units.
patternLength :: Automaton -> CodeUnitIndex
patternLength = lengthUtf16 . patternText

-- | Return the pattern that was used to construct the automaton.
patternText :: Automaton -> Text
patternText (Automaton pattern _ _) = unhashed pattern

-- | The suffix table tells us for each character of the pattern how many characters we can
-- jump ahead if the match fails at that point.
newtype SuffixTable = SuffixTable (UVector.Vector Int)
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

-- | Lookup an entry in the suffix table.
suffixLookup :: SuffixTable -> CodeUnitIndex -> CodeUnitIndex
{-# INLINE suffixLookup #-}
suffixLookup (SuffixTable table) = CodeUnitIndex . indexTable table . codeUnitIndex

buildSuffixTable :: Text -> SuffixTable
buildSuffixTable pattern = runST $ do
  let patLen = lengthUtf16 pattern

  table <- UMVector.new $ codeUnitIndex patLen

  let
    -- Case 1: For each position of the pattern we record the shift that would align the pattern so
    -- that it starts at the longest suffix that is at the same time a prefix, if a mismatch would
    -- happen at that position.
    --
    -- Suppose the length of the pattern is n, a mismatch occurs at position i in the pattern and j
    -- in the haystack, then we know that pattern[i+1..n] == haystack[j+1..j+n-i]. That is, we know
    -- that the part of the haystack that we already matched is a suffix of the pattern.
    -- If the pattern happens to have a prefix that is equal to or a shorter suffix of that matched
    -- suffix, we can shift the pattern to the right so that the pattern starts at the longest
    -- suffix that we have seen that conincides with a prefix of the pattern.
    --
    -- Consider the pattern `ababa`. Then we get
    --
    -- p:                0  1  2  3  4
    -- Pattern:          a  b  a  b  a
    -- lastPrefixIndex:  2  2  4  4  5
    -- table:            6  5  6  5  5
    init1 lastPrefixIndex p
      | p >= 0 = do
        let
          prefixIndex
            | isPrefix pattern (p + 1) = p + 1
            | otherwise = lastPrefixIndex
        UMVector.write table (codeUnitIndex p) (codeUnitIndex $ prefixIndex + patLen - 1 - p)
        init1 prefixIndex (p - 1)
      | otherwise = pure ()

    -- Case 2: We also have to account for the fact that the matching suffix of the pattern might
    -- occur again somewhere within the pattern. In that case, we may not shift as far as if it was
    -- a prefix. That is why the `init2` loop is run after `init1`, potentially overwriting some
    -- entries with smaller shifts.
    init2 p
      | p < patLen - 1 = do
        let
          suffixLen = suffixLength pattern p
        when (indexCodePoint pattern (p - suffixLen) /= indexCodePoint pattern (patLen - 1 - suffixLen)) $
          UMVector.write table (codeUnitIndex $ patLen - 1 - suffixLen) (codeUnitIndex $ patLen - 1 - p + suffixLen)
        init2 (p + 1)
      | otherwise = pure ()

  init1 (patLen - 1) (patLen - 1)
  init2 0

  SuffixTable <$> UVector.unsafeFreeze table


-- | The bad char table tells us how far we may skip ahead when encountering a certain character
-- in the input string. For example, if there's a character that is not contained in the pattern at
-- all, we can skip ahead until after that character.
data BadCharTable = BadCharTable
  { badCharTableAscii :: {-# UNPACK #-} !(UVector.Vector Int)
    -- ^ The element type should be CodeUnitIndex, but there's no unboxed vector for that type, and
    -- defining it would be a lot of boilerplate.
  , badCharTableNonAscii :: !(HashMap.HashMap CodeUnit CodeUnitIndex)
  , badCharTablePatternLen :: CodeUnitIndex
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

-- | Number of entries in the fixed-size lookup-table of the bad char table.
asciiCount :: Int
{-# INLINE asciiCount #-}
asciiCount = 128

-- | Lookup an entry in the bad char table.
badCharLookup :: BadCharTable -> CodeUnit -> CodeUnitIndex
{-# INLINE badCharLookup #-}
badCharLookup (BadCharTable asciiTable nonAsciis patLen) char
  | intChar < asciiCount = CodeUnitIndex $ indexTable asciiTable intChar
  | otherwise = HashMap.lookupDefault patLen char nonAsciis
  where
    intChar = fromIntegral char

-- | True if the suffix of the @pattern@ starting from @pos@ is a prefix of the pattern
-- For example, @isPrefix \"aabbaa\" 4 == True@.
isPrefix :: Text -> CodeUnitIndex -> Bool
isPrefix pattern pos = go 0
  where
    suffixLen = lengthUtf16 pattern - pos
    go i
      | i < suffixLen =
        if indexCodePoint pattern i == indexCodePoint pattern (pos + i)
          then go (i + 1)
          else False
      | otherwise = True

-- | Length of the longest suffix of the pattern ending on @pos@.
-- For example, @suffixLength \"abaacbbaac\" 4 == 4@, because the substring \"baac\" ends at position
-- 4 and is at the same time the longest suffix that does so, having length 4.
suffixLength :: Text -> CodeUnitIndex -> CodeUnitIndex
suffixLength pattern pos = go 0
  where
    patLen = lengthUtf16 pattern
    go i
      | indexCodePoint pattern (pos - i) == indexCodePoint pattern (patLen - 1 - i) && i < pos = go (i + 1)
      | otherwise = i

buildBadCharTable :: Text -> BadCharTable
buildBadCharTable pattern = runST $ do
  let patLen = lengthUtf16 pattern

  -- Initialize table with the maximum skip distance, which is the length of the pattern.
  -- This applies to all characters that are not part of the pattern.
  asciiTable <- UMVector.replicate asciiCount $ codeUnitIndex patLen

  let
    -- Fill the bad character table based on the rightmost occurrence of a character in the pattern.
    -- Note that there is also a variant of Boyer-Moore that records all positions (see Wikipedia,
    -- but that requires even more storage space).
    -- Also note that we exclude the last character of the pattern when building the table.
    -- This is because
    --
    -- 1. If the last character does not occur anywhere else in the pattern and we encounter it
    --    during a mismatch, we can advance the pattern to just after that character:
    --
    --    Haystack: aaadcdabcdbb
    --    Pattern:    abcd
    --
    --    In the above example, we would match `d` and `c`, but then fail because `d` != `b`.
    --    Since `d` only occurs at the very last position of the pattern, we can shift to
    --
    --    Haystack: aaadcdabcdbb
    --    Pattern:      abcd
    --
    -- 2. If it does occur anywhere else in the pattern, we can only shift as far as it's necessary
    --    to align it with the haystack:
    --
    --    Haystack: aaadddabcdbb
    --    Pattern:    adcd
    --
    --    We match `d`, and then there is a mismatch `d` != `c`, which allows us to shift only up to:

    --    Haystack: aaadddabcdbb
    --    Pattern:     adcd
    fillTable !i !nonAsciis
      -- for(i = 0; i < patLen - 1; i++) {
      | i < patLen - 1 = do
        let patChar = indexCodePoint pattern i
        if fromIntegral patChar < asciiCount
          then do
            UMVector.write asciiTable (fromIntegral patChar) (codeUnitIndex $ patLen - 1 - i)
            fillTable (i + 1) nonAsciis
          else
            fillTable (i + 1) (HashMap.insert patChar (patLen - 1 - i) nonAsciis)
      -- }
      | otherwise = pure nonAsciis

  nonAsciis <- fillTable 0 HashMap.empty

  asciiTableFrozen <- UVector.unsafeFreeze asciiTable

  pure BadCharTable
    { badCharTableAscii = asciiTableFrozen
    , badCharTableNonAscii = nonAsciis
    , badCharTablePatternLen = patLen
    }


-- Helper functions for easily toggling the safety of this module

-- | Read from a lookup table at the specified index.
indexTable :: UVector.Unbox a => UVector.Vector a -> Int -> a
{-# INLINE indexTable #-}
indexTable = UVector.unsafeIndex

-- | Read from a lookup table at the specified index.
indexCodePoint :: Text -> CodeUnitIndex -> CodeUnit
{-# INLINE indexCodePoint #-}
indexCodePoint text index = unsafeIndexUtf16 text index
  {-
  | index < 0 || index >= lengthUtf16 text = error $ "Index out of bounds " ++ show index
  | otherwise = unsafeIndexUtf16 text index
  -}
