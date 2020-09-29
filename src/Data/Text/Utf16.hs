-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fllvm -O2 -optlo=-O3 -optlo=-tailcallelim -fignore-asserts #-}

-- | This module provides functions that allow treating Text values as series of Utf16 codepoints
-- instead of characters.
module Data.Text.Utf16
  ( CodeUnit
  , CodeUnitIndex (..)
  , lengthUtf16
  , lowerUtf16
  , lowerCodeUnit
  , isCaseInvariant
  , unpackUtf16
  , unsafeCutUtf16
  , unsafeSliceUtf16
  , unsafeIndexUtf16
  , indexTextArray
  ) where

import Prelude hiding (length)

import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Data.Hashable (Hashable)
import Data.Primitive.ByteArray (ByteArray (..), sizeofByteArray)
import Data.Text.Internal (Text (..))
import Data.Word (Word16)
import GHC.Generics (Generic)

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Unsafe as TextUnsafe
import qualified Data.Vector.Primitive as PVector

-- | A code unit is a 16-bit integer from which UTF-16 encoded text is built up.
-- The `Text` type is represented as a UTF-16 string.
type CodeUnit = Word16

-- | An index into the raw UTF-16 data of a `Text`. This is not the code point
-- index as conventionally accepted by `Text`, so we wrap it to avoid confusing
-- the two. Incorrect index manipulation can lead to surrogate pairs being
-- sliced, so manipulate indices with care. This type is also used for lengths.
newtype CodeUnitIndex = CodeUnitIndex
  { codeUnitIndex :: Int
  }
  deriving stock (Eq, Ord, Show, Generic, Bounded)
  deriving newtype (Hashable, Num, NFData)

-- | Return a Text as a list of UTF-16 code units.
{-# INLINABLE unpackUtf16 #-}
unpackUtf16 :: Text -> [CodeUnit]
unpackUtf16 (Text u16data offset length) =
  let
    go _ 0 = []
    go i n = indexTextArray u16data i : go (i + 1) (n - 1)
  in
    go offset length

-- | Return whether the code unit at the given index starts a surrogate pair.
-- Such a code unit must be followed by a high surrogate in valid UTF-16.
-- Returns false on out of bounds indices.
{-# INLINE isLowSurrogate #-}
isLowSurrogate :: Int -> Text -> Bool
isLowSurrogate !i (Text !u16data !offset !len) =
  let
    w = indexTextArray u16data (offset + i)
  in
    i >= 0 && i < len && w >= 0xd800 && w <= 0xdbff

-- | Return whether the code unit at the given index ends a surrogate pair.
-- Such a code unit must be preceded by a low surrogate in valid UTF-16.
-- Returns false on out of bounds indices.
{-# INLINE isHighSurrogate #-}
isHighSurrogate :: Int -> Text -> Bool
isHighSurrogate !i (Text !u16data !offset !len) =
  let
    w = indexTextArray u16data (offset + i)
  in
    i >= 0 && i < len && w >= 0xdc00 && w <= 0xdfff

-- | Extract a substring from a text, at a code unit offset and length.
-- This is similar to `Text.take length . Text.drop begin`, except that the
-- begin and length are in code *units*, not code points, so we can slice the
-- UTF-16 array, and we don't have to walk the entire text to take surrogate
-- pairs into account. It is the responsibility of the user to not slice
-- surrogate pairs, and to ensure that the length is within bounds, hence this
-- function is unsafe.
{-# INLINE unsafeSliceUtf16 #-}
unsafeSliceUtf16 :: CodeUnitIndex -> CodeUnitIndex -> Text -> Text
unsafeSliceUtf16 (CodeUnitIndex !begin) (CodeUnitIndex !length) !text
  = assert (begin + length <= TextUnsafe.lengthWord16 text)
  $ assert (not $ isHighSurrogate begin text)
  $ assert (not $ isLowSurrogate (begin + length - 1) text)
  $ TextUnsafe.takeWord16 length $ TextUnsafe.dropWord16 begin text

-- | The complement of `unsafeSliceUtf16`: removes the slice, and returns the
-- part before and after. See `unsafeSliceUtf16` for details.
{-# INLINE unsafeCutUtf16 #-}
unsafeCutUtf16 :: CodeUnitIndex -> CodeUnitIndex -> Text -> (Text, Text)
unsafeCutUtf16 (CodeUnitIndex !begin) (CodeUnitIndex !length) !text
  = assert (begin + length <= TextUnsafe.lengthWord16 text)
  $ assert (not $ isHighSurrogate begin text)
  $ assert (not $ isLowSurrogate (begin + length - 1) text)
    ( TextUnsafe.takeWord16 begin text
    , TextUnsafe.dropWord16 (begin + length) text
    )

-- | Return the length of the text, in number of code units.
{-# INLINE lengthUtf16 #-}
lengthUtf16 :: Text -> CodeUnitIndex
lengthUtf16 = CodeUnitIndex . TextUnsafe.lengthWord16

-- | Return the code unit (not character) with the given index.
-- Note: The boudns are not checked.
unsafeIndexUtf16 :: Text -> CodeUnitIndex -> CodeUnit
{-# INLINE unsafeIndexUtf16 #-}
unsafeIndexUtf16 (Text arr off _) (CodeUnitIndex pos) = indexTextArray arr (pos + off)

-- | Apply a function to each code unit of a text.
{-# INLINABLE mapUtf16 #-}
mapUtf16 :: (CodeUnit -> CodeUnit) -> Text -> Text
mapUtf16 f (Text u16data offset length) =
  let
    get !i = f $ indexTextArray u16data (offset + i)
    !(PVector.Vector !offset' !length' !(ByteArray !u16data')) =
      PVector.generate length get
  in
    Text (TextArray.Array u16data') offset' length'

-- | Lowercase each individual code unit of a text without changing their index.
-- This is not a proper case folding, but it does ensure that indices into the
-- lowercased string correspond to indices into the original string.
--
-- Differences from `Text.toLower` include code points in the BMP that lowercase
-- to multiple code points, and code points outside of the BMP.
--
-- For example, "İ" (U+0130), which `toLower` converts to "i" (U+0069, U+0307),
-- is converted into U+0069 only by `lowerUtf16`.
-- Also, "𑢢" (U+118A2), a code point from the Warang City writing system in the
-- Supplementary Multilingual Plane, introduced in 2014 to Unicode 7. It would
-- be lowercased to U+118C2 by `toLower`, but it is left untouched by
-- `lowerUtf16`.
{-# INLINE lowerUtf16 #-}
lowerUtf16 :: Text -> Text
lowerUtf16 = mapUtf16 lowerCodeUnit

-- | Convert CodeUnits that represent a character on their own (i.e. that are not part of a
-- surrogate pair) to their lower case representation.
--
-- This function has a special code path for ASCII characters, because Char.toLower
-- is **incredibly** slow. It's implemented there if you want to see for yourself:
-- (https://github.com/ghc/ghc/blob/ghc-8.6.3-release/libraries/base/cbits/WCsubst.c#L4732)
-- (It does a binary search on 1276 casing rules)
{-# INLINE lowerCodeUnit #-}
lowerCodeUnit :: CodeUnit -> CodeUnit
lowerCodeUnit cu
  -- ASCII letters A..Z and a..z are two contiguous blocks.
  -- Converting to lower case amounts to adding a fixed offset.
  | fromIntegral cu >= Char.ord 'A' && fromIntegral cu <= Char.ord 'Z'
    = cu + fromIntegral (Char.ord 'a' - Char.ord 'A')

    -- Everything else in ASCII is invariant under toLower.
  -- The a..z range is already lower case, and all non-letter characters are case-invariant.
  | cu <= 127 = cu

  -- This code unit is part of a surrogate pair. Don't touch those, because
  -- we don't have all information required to decode the code point. Note
  -- that alphabets that need to be encoded as surrogate pairs are mostly
  -- archaic and obscure; all of the languages used by our customers have
  -- alphabets in the Basic Multilingual Plane, which does not need surrogate
  -- pairs. Note that the BMP is not just ascii or extended ascii. See also
  -- https://codepoints.net/basic_multilingual_plane.
  | cu >= 0xd800 && cu < 0xe000 = cu

  -- The code unit is a code point on its own (not part of a surrogate pair),
  -- lowercase the code point. These code points, which are all in the BMP,
  -- have the important property that lowercasing them is again a code point
  -- in the BMP, so the output can be encoded in exactly one code unit, just
  -- like the input. This property was verified by exhaustive testing; see
  -- also the test in AhoCorasickSpec.hs.
  | otherwise = fromIntegral $ Char.ord $ Char.toLower $ Char.chr $ fromIntegral cu

-- | Return whether text is the same lowercase as uppercase, such that this
-- function will not return true when Aho–Corasick would differentiate when
-- doing case-insensitive matching.
{-# INLINE isCaseInvariant #-}
isCaseInvariant :: Text -> Bool
isCaseInvariant = Text.all (\c -> Char.toLower c == Char.toUpper c)

{-# INLINE indexTextArray #-}
indexTextArray :: TextArray.Array -> Int -> CodeUnit
indexTextArray array@(TextArray.Array byteArray) index
  = assert (2 * index < sizeofByteArray (ByteArray byteArray))
  $ assert (0 <= index)
  $ TextArray.unsafeIndex array index
