-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2022 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module provides functions that allow treating 'Text' values as series of UTF-8 code units
-- instead of characters. Currently, it also contains a stub 'Text' type which treats its internal byte array
-- as UTF-8 encoded. We use this as a placeholder until we can use @text-2@.
module Data.Text.Utf8
    ( CodePoint
    , CodeUnit
    , CodeUnitIndex (..)
    , Text (..)
    , fromByteList
    , lengthUtf8
    , lowerCodePoint
    , lowerUtf8
    , toLowerAscii
    , unicode2utf8
    , unpackUtf8
      -- * Decoding
      --
      -- $decoding
    , decode2
    , decode3
    , decode4
    , decodeUtf8
      -- * Indexing
      --
      -- $indexing
    , indexCodeUnit
    , unsafeIndexCodePoint
    , unsafeIndexCodePoint'
    , unsafeIndexCodeUnit
    , unsafeIndexCodeUnit'
      -- * Slicing Functions
      --
      -- $slicingFunctions
    , unsafeCutUtf8
    , unsafeSliceUtf8
      -- * General Functions
      --
      -- $generalFunctions
    , Text.concat
    , Text.dropWhile
    , Text.isInfixOf
    , Text.null
    , Text.pack
    , Text.replicate
    , Text.unpack
    , TextIO.readFile
    , TextSearch.indices
    ) where

import Control.DeepSeq (NFData)
import Data.Bits (Bits (shiftL), shiftR, (.&.), (.|.))
import Data.Hashable (Hashable)
import Data.Text.Internal (Text (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Data.Primitive (ByteArray(ByteArray), byteArrayFromList)
#if defined(HAS_AESON)
import Data.Aeson (FromJSON, ToJSON)
#endif

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Internal.Search as TextSearch
import qualified Data.Text.Unsafe as TextUnsafe

-- | A UTF-8 code unit is a byte. A Unicode code point can be encoded as up to four code units.
type CodeUnit = Word8

-- | A Unicode code point.
type CodePoint = Char

-- | An index into the raw UTF-8 data of a `Text`. This is not the code point
-- index as conventionally accepted by `Text`, so we wrap it to avoid confusing
-- the two. Incorrect index manipulation can lead to surrogate pairs being
-- sliced, so manipulate indices with care. This type is also used for lengths.
newtype CodeUnitIndex = CodeUnitIndex
    { codeUnitIndex :: Int
    }
    deriving stock (Eq, Ord, Show, Generic, Bounded)
#if defined(HAS_AESON)
    deriving newtype (Hashable, Num, NFData, FromJSON, ToJSON)
#else
    deriving newtype (Hashable, Num, NFData)
#endif

{-# INLINABLE unpackUtf8 #-}
unpackUtf8 :: Text -> [CodeUnit]
unpackUtf8 (Text u8data offset length) =
  let
    go _ 0 = []
    go i n = unsafeIndexCodeUnit' u8data (CodeUnitIndex i) : go (i + 1) (n - 1)
  in
    go offset length

-- | The return value of this function is not really an index.
-- However the signature is supposed to make it clear that the length is returned in terms of code units, not code points.
lengthUtf8 :: Text -> CodeUnitIndex
lengthUtf8 (Text _ _ !length) = CodeUnitIndex length

-- | Lower-case the ASCII code points A-Z and leave the rest of ASCII intact.
{-# INLINE toLowerAscii #-}
toLowerAscii :: Char -> Char
toLowerAscii cp
  | Char.isAsciiUpper cp = Char.chr (Char.ord cp + 0x20)
  | otherwise = cp

-- | Lowercase a 'Text' by applying 'lowerCodePoint' to each 'Char'.
{-# INLINE lowerUtf8 #-}
lowerUtf8 :: Text -> Text
lowerUtf8 = Text.map lowerCodePoint

asciiCount :: Int
asciiCount = 128

{-# INLINE lowerCodePoint #-}
-- | Lower-Case a UTF-8 codepoint.
-- Uses 'toLowerAscii' for ASCII and 'Char.toLower' otherwise.
lowerCodePoint :: Char -> Char
lowerCodePoint cp
  | Char.ord cp < asciiCount = toLowerAscii cp
  | otherwise = Char.toLower cp

-- | Convert a Unicode Code Point 'c' into a list of UTF-8 code units (bytes).
unicode2utf8 :: (Ord a, Num a, Bits a) => a -> [a]
unicode2utf8 c
    | c < 0x80    = [c]
    | c < 0x800   = [0xc0 .|. (c `shiftR` 6), 0x80 .|. (0x3f .&. c)]
    | c < 0x10000 = [0xe0 .|. (c `shiftR` 12), 0x80 .|. (0x3f .&. (c `shiftR` 6)), 0x80 .|. (0x3f .&. c)]
    | otherwise   = [0xf0 .|. (c `shiftR` 18), 0x80 .|. (0x3f .&. (c `shiftR` 12)), 0x80 .|. (0x3f .&. (c `shiftR` 6)), 0x80 .|. (0x3f .&. c)]

fromByteList :: [Word8] -> Text
fromByteList byteList = Text (TextArray.ByteArray ba) 0 (length byteList)
  where ByteArray ba = byteArrayFromList byteList

-- $decoding
--
-- Functions that turns code unit sequences into code point sequences.

-- | Decode 2 UTF-8 code units into their code point.
-- The given code units should have the following format:
--
-- > ┌───────────────┬───────────────┐
-- > │1 1 0 x x x x x│1 0 x x x x x x│
-- > └───────────────┴───────────────┘
{-# INLINE decode2 #-}
decode2 :: CodeUnit -> CodeUnit -> CodePoint
decode2 cu0 cu1 =
  Char.chr $ (fromIntegral cu0 .&. 0x1f) `shiftL` 6 .|. fromIntegral cu1 .&. 0x3f

-- | Decode 3 UTF-8 code units into their code point.
-- The given code units should have the following format:
--
-- > ┌───────────────┬───────────────┬───────────────┐
-- > │1 1 1 0 x x x x│1 0 x x x x x x│1 0 x x x x x x│
-- > └───────────────┴───────────────┴───────────────┘
{-# INLINE decode3 #-}
decode3 :: CodeUnit -> CodeUnit -> CodeUnit -> CodePoint
decode3 cu0 cu1 cu2 =
  Char.chr $ (fromIntegral cu0 .&. 0xf) `shiftL` 12 .|. (fromIntegral cu1 .&. 0x3f) `shiftL` 6 .|. (fromIntegral cu2 .&. 0x3f)

-- | Decode 4 UTF-8 code units into their code point.
-- The given code units should have the following format:
--
-- > ┌───────────────┬───────────────┬───────────────┬───────────────┐
-- > │1 1 1 1 0 x x x│1 0 x x x x x x│1 0 x x x x x x│1 0 x x x x x x│
-- > └───────────────┴───────────────┴───────────────┴───────────────┘
{-# INLINE decode4 #-}
decode4 :: CodeUnit -> CodeUnit -> CodeUnit -> CodeUnit -> CodePoint
decode4 cu0 cu1 cu2 cu3 =
  Char.chr $ (fromIntegral cu0 .&. 0x7) `shiftL` 18 .|. (fromIntegral cu1 .&. 0x3f) `shiftL` 12 .|. (fromIntegral cu2 .&. 0x3f) `shiftL` 6 .|. (fromIntegral cu3 .&. 0x3f)

-- | Decode a list of UTF-8 code units into a list of code points.
decodeUtf8 :: [CodeUnit] -> [CodePoint]
decodeUtf8 [] = []
decodeUtf8 (cu0 : cus) | cu0 < 0xc0 = Char.chr (fromIntegral cu0) : decodeUtf8 cus
decodeUtf8 (cu0 : cu1 : cus) | cu0 < 0xe0 = decode2 cu0 cu1 : decodeUtf8 cus
decodeUtf8 (cu0 : cu1 : cu2 : cus) | cu0 < 0xf0 = decode3 cu0 cu1 cu2 : decodeUtf8 cus
decodeUtf8 (cu0 : cu1 : cu2 : cu3 : cus) | cu0 < 0xf8 = decode4 cu0 cu1 cu2 cu3 : decodeUtf8 cus
decodeUtf8 cus = error $ "Invalid UTF-8 input sequence at " ++ show (take 4 cus)

-- $indexing
--
-- 'Text' can be indexed by code units or code points.
-- A 'CodePoint' is a 21-bit Unicode code point and can consist of up to four code units.
-- A 'CodeUnit' is a single byte.

-- | Decode a code point at the given 'CodeUnitIndex'.
-- Returns garbage if there is no valid code point at that position.
-- Does not perform bounds checking.
-- See 'decode2', 'decode3' and 'decode4' for the expected format of multi-byte code points.
{-# INLINE unsafeIndexCodePoint' #-}
unsafeIndexCodePoint' :: TextArray.Array -> CodeUnitIndex -> (CodeUnitIndex, CodePoint)
unsafeIndexCodePoint' !u8data (CodeUnitIndex !idx)
  | cu0 < 0xc0 = (1, Char.chr $ fromIntegral cu0)
  | cu0 < 0xe0 = (2, decode2 cu0 (cuAt 1))
  | cu0 < 0xf0 = (3, decode3 cu0 (cuAt 1) (cuAt 2))
  | otherwise = (4, decode4 cu0 (cuAt 1) (cuAt 2) (cuAt 3))
  where
    cuAt !i = unsafeIndexCodeUnit' u8data $ CodeUnitIndex $ idx + i
    !cu0 = cuAt 0

-- | Does exactly the same thing as 'unsafeIndexCodePoint'', but on 'Text' values.
{-# INLINE unsafeIndexCodePoint #-}
unsafeIndexCodePoint :: Text -> CodeUnitIndex -> (CodeUnitIndex, CodePoint)
unsafeIndexCodePoint (Text !u8data !off !_len) (CodeUnitIndex !index) =
  unsafeIndexCodePoint' u8data $ CodeUnitIndex $ off + index

-- | Get the code unit at the given 'CodeUnitIndex'.
-- Performs bounds checking.
{-# INLINE indexCodeUnit #-}
indexCodeUnit :: Text -> CodeUnitIndex -> CodeUnit
indexCodeUnit !text (CodeUnitIndex !index)
  | index < 0 || index >= codeUnitIndex (lengthUtf8 text) = error $ "Index out of bounds " ++ show index
  | otherwise = unsafeIndexCodeUnit text $ CodeUnitIndex index

{-# INLINE unsafeIndexCodeUnit' #-}
unsafeIndexCodeUnit' :: TextArray.Array -> CodeUnitIndex -> CodeUnit
unsafeIndexCodeUnit' !u8data (CodeUnitIndex !idx) = TextArray.unsafeIndex u8data idx

{-# INLINE unsafeIndexCodeUnit #-}
unsafeIndexCodeUnit :: Text -> CodeUnitIndex -> CodeUnit
unsafeIndexCodeUnit (Text !u8data !off !_len) (CodeUnitIndex !index) =
  unsafeIndexCodeUnit' u8data $ CodeUnitIndex $ off + index

-- $slicingFunctions
--
-- 'unsafeCutUtf8' and 'unsafeSliceUtf8' are used to retrieve slices of 'Text' values.
-- @unsafeSliceUtf8 begin length@ returns a substring of length @length@ starting at @begin@.
-- @unsafeSliceUtf8 begin length@ returns a tuple of the "surrounding" substrings.
--
-- They satisfy the following property:
--
-- > let (prefix, suffix) = unsafeCutUtf8 begin length t
-- > in concat [prefix, unsafeSliceUtf8 begin length t, suffix] == t
--
-- The following diagram visualizes the relevant offsets for @begin = CodeUnitIndex 2@, @length = CodeUnitIndex 6@ and @t = \"BCDEFGHIJKL\"@.
--
-- >  off                 off+len
-- >   │                     │
-- >   ▼                     ▼
-- > ──┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬──
-- >  A│B│C│D│E│F│G│H│I│J│K│L│M│N
-- > ──┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴──
-- >       ▲           ▲
-- >       │           │
-- >  off+begin   off+begin+length
-- >
-- > unsafeSliceUtf8 begin length t == "DEFGHI"
-- > unsafeCutUtf8 begin length t == ("BC", "JKL")
--
-- The shown array is open at each end because in general, @t@ may be a slice as well.
--
-- __WARNING__: As their name implies, these functions are not (necessarily) bounds-checked. Use at your own risk.

unsafeCutUtf8 :: CodeUnitIndex -- ^ Starting position of substring.
  -> CodeUnitIndex -- ^ Length of substring.
  -> Text -- ^ Initial string.
  -> (Text, Text)
unsafeCutUtf8 (CodeUnitIndex !begin) (CodeUnitIndex !length) !text =
  ( TextUnsafe.takeWord8 begin text
  , TextUnsafe.dropWord8 (begin + length) text
  )

unsafeSliceUtf8 :: CodeUnitIndex -> CodeUnitIndex -> Text -> Text
unsafeSliceUtf8 (CodeUnitIndex !begin) (CodeUnitIndex !length) !text =
  TextUnsafe.takeWord8 length $ TextUnsafe.dropWord8 begin text

-- $generalFunctions
--
-- These functions are available in @text@ as well and should be removed once this library moves to @text-2@.
-- You should be able to use these by doing @import qualified Data.Text.Utf8 as Text@ just like you would with @text@.
--
-- NOTE: The 'Text' instances for @Show@, @Eq@, @Ord@, @IsString@, @FromJSON@, @ToJSON@ and @Hashable@ in this file also fall in this category.
