-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2022 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE UnboxedTuples #-}

-- | This module provides functions that allow treating 'Text' values as series of UTF-8 code units
-- instead of characters. Any calls to 'Text' in @alfred-margaret@ go through this module.
-- Therefore we re-export some 'Text' functions, e.g. 'Text.concat'.
module Data.Text.Utf8
    ( CodePoint
    , CodeUnit
    , CodeUnitIndex (..)
    , Text (..)
    , fromByteList
    , isCaseInvariant
    , lengthUtf8
    , lowerCodePoint
    , unlowerCodePoint
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
    , unsafeIndexCodeUnit
    , skipCodePointsBackwards
      -- * Slicing Functions
      --
      -- $slicingFunctions
    , unsafeCutUtf8
    , unsafeSliceUtf8
      -- * Functions on Arrays
      --
      -- $functionsOnArrays
    , arrayContents
    , isArrayPinned
    , unsafeIndexCodePoint'
    , unsafeIndexCodeUnit'
    , BackwardsIter (..)
    , unsafeIndexEndOfCodePoint'
    , unsafeIndexAnywhereInCodePoint'

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
    , TextSearch.indices
    ) where

import Control.DeepSeq (NFData)
import Data.Bits (Bits (shiftL), shiftR, (.&.), (.|.))
import Data.Hashable (Hashable)
import Data.Text.Internal (Text (..))
import Data.Word (Word8)
import GHC.Generics (Generic)
import Data.Primitive (ByteArray (ByteArray), Prim, byteArrayFromList)
#if defined(HAS_AESON)
import Data.Aeson (FromJSON, ToJSON)
#endif
import Data.Text.Utf8.Unlower (unlowerCodePoint)

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Internal.Search as TextSearch
import qualified Data.Text.Unsafe as TextUnsafe
import qualified GHC.Exts as Exts

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
    deriving stock (Eq, Ord, Generic, Bounded)
#if defined(HAS_AESON)
    deriving newtype (Show, Prim, Hashable, Num, NFData, FromJSON, ToJSON)
#else
    deriving newtype (Show, Prim, Hashable, Num, NFData)
#endif

{-# INLINABLE unpackUtf8 #-}
unpackUtf8 :: Text -> [CodeUnit]
unpackUtf8 (Text u8data offset len) =
  let
    go _ 0 = []
    go i n = unsafeIndexCodeUnit' u8data (CodeUnitIndex i) : go (i + 1) (n - 1)
  in
    go offset len

-- | The return value of this function is not really an index.
-- However the signature is supposed to make it clear that the length is returned in terms of code units, not code points.
lengthUtf8 :: Text -> CodeUnitIndex
lengthUtf8 (Text _ _ !len) = CodeUnitIndex len

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
{-# INLINE unicode2utf8 #-}
unicode2utf8 c
    | c < 0x80    = [c]
    | c < 0x800   = [0xc0 .|. (c `shiftR` 6), 0x80 .|. (0x3f .&. c)]
    | c < 0x10000 = [0xe0 .|. (c `shiftR` 12), 0x80 .|. (0x3f .&. (c `shiftR` 6)), 0x80 .|. (0x3f .&. c)]
    | otherwise   = [0xf0 .|. (c `shiftR` 18), 0x80 .|. (0x3f .&. (c `shiftR` 12)), 0x80 .|. (0x3f .&. (c `shiftR` 6)), 0x80 .|. (0x3f .&. c)]

fromByteList :: [Word8] -> Text
fromByteList byteList = Text (TextArray.ByteArray ba#) 0 (length byteList)
  where !(ByteArray ba#) = byteArrayFromList byteList

-- | Return whether text has exactly one case variation, such that this function
-- will not return true when Aho–Corasick would differentiate when doing
-- case-insensitive matching.
{-# INLINE isCaseInvariant #-}
isCaseInvariant :: Text -> Bool
isCaseInvariant = Text.all (\c -> unlowerCodePoint (lowerCodePoint c) == [c])

-- $decoding
--
-- Functions that turns code unit sequences into code point sequences.

-- | Decode a single UTF-8 code unit into its code point.
-- The given code unit should have the following format:
--
-- > ┌───────────────┐
-- > │0 x x x x x x x│
-- > └───────────────┘
decode1 :: CodeUnit -> CodePoint
decode1 cu0 =
  Char.chr $ fromIntegral cu0

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

-- | Does exactly the same thing as 'unsafeIndexCodePoint'', but on 'Text' values.
{-# INLINE unsafeIndexCodePoint #-}
unsafeIndexCodePoint :: Text -> CodeUnitIndex -> (CodeUnitIndex, CodePoint)
unsafeIndexCodePoint (Text !u8data !off !_len) !index =
  unsafeIndexCodePoint' u8data $ CodeUnitIndex off + index

-- | Get the code unit at the given 'CodeUnitIndex'.
-- Performs bounds checking.
{-# INLINE indexCodeUnit #-}
indexCodeUnit :: Text -> CodeUnitIndex -> CodeUnit
indexCodeUnit !text !index
  | index < 0 || index >= lengthUtf8 text = error $ "Index out of bounds " ++ show index
  | otherwise = unsafeIndexCodeUnit text index

{-# INLINE unsafeIndexCodeUnit #-}
unsafeIndexCodeUnit :: Text -> CodeUnitIndex -> CodeUnit
unsafeIndexCodeUnit (Text !u8data !off !_len) !index =
  unsafeIndexCodeUnit' u8data $ CodeUnitIndex off + index

-- | Scan backwards through the text until we've seen the specified number of codepoints. Assumes
-- that the initial CodeUnitIndex is within a codepoint.
{-# INLINE skipCodePointsBackwards #-}
skipCodePointsBackwards :: Text -> CodeUnitIndex -> Int -> CodeUnitIndex
skipCodePointsBackwards (Text !u8data !off !len) !index0 !n0
  | index0 >= CodeUnitIndex len = error "Invalid use of skipCodePointsBackwards"
  | otherwise = loop (index0 + CodeUnitIndex off) n0
  where
    loop index n | atTrailingByte index =
      loop (index-1) n  -- Don't exit before we're at a leading byte
    loop index 0 | index < 0 =
      -- Throw an error if we've read before the array (e.g. when the data was
      -- not valid UTF-8), this one-time check doesn't prevent undefined
      -- behaviour but may help you locate bugs.
      error "Invalid use of skipCodePointsBackwards"
    loop index 0 =
      index - CodeUnitIndex off
    loop index n =
      loop (index-1) (n-1)

    -- Second, third and fourth bytes of a codepoint are always 10xxxxxx, while
    -- the first byte can be 0xxxxxxx or 11yyyyyy.
    atTrailingByte !index = unsafeIndexCodeUnit' u8data index .&. 0b1100_0000 == 0b1000_0000

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
unsafeCutUtf8 (CodeUnitIndex !begin) (CodeUnitIndex !len) !text =
  ( TextUnsafe.takeWord8 begin text
  , TextUnsafe.dropWord8 (begin + len) text
  )

unsafeSliceUtf8 :: CodeUnitIndex -> CodeUnitIndex -> Text -> Text
unsafeSliceUtf8 (CodeUnitIndex !begin) (CodeUnitIndex !len) !text =
  TextUnsafe.takeWord8 len $ TextUnsafe.dropWord8 begin text

-- $functionsOnArrays
--
-- Functions for working with 'TextArray.Array' values.

-- | See 'Data.Primitive.isByteArrayPinned'.
isArrayPinned :: TextArray.Array -> Bool
isArrayPinned (TextArray.ByteArray ba#) = Exts.isTrue# (Exts.isByteArrayPinned# ba#)

-- | See 'Data.Primitive.byteArrayContents'.
arrayContents :: TextArray.Array -> Exts.Ptr Word8
arrayContents (TextArray.ByteArray ba#) = Exts.Ptr (Exts.byteArrayContents# ba#)

-- | Decode a code point at the given 'CodeUnitIndex'.
-- Returns garbage if there is no valid code point at that position.
-- Does not perform bounds checking.
-- See 'decode2', 'decode3' and 'decode4' for the expected format of multi-byte code points.
unsafeIndexCodePoint' :: TextArray.Array -> CodeUnitIndex -> (CodeUnitIndex, CodePoint)
{-# INLINE unsafeIndexCodePoint' #-}
unsafeIndexCodePoint' !u8data !idx =
  decodeN (cuAt 0) (cuAt 1) (cuAt 2) (cuAt 3)
  where
    cuAt !i = unsafeIndexCodeUnit' u8data $ idx + i

decodeN :: CodeUnit -> CodeUnit -> CodeUnit -> CodeUnit -> (CodeUnitIndex, CodePoint)
{-# INLINE decodeN #-}
decodeN cu0 cu1 cu2 cu3
  | cu0 < 0xc0 = (1, decode1 cu0)
  | cu0 < 0xe0 = (2, decode2 cu0 cu1)
  | cu0 < 0xf0 = (3, decode3 cu0 cu1 cu2)
  | otherwise = (4, decode4 cu0 cu1 cu2 cu3)



-- | Intermediate state when you're iterating backwards through a Utf8 text.
data BackwardsIter = BackwardsIter
  { backwardsIterNext :: {-# UNPACK #-} !CodeUnitIndex
    -- ^ First byte to the left of the codepoint that we're focused on. This can
    -- be used with 'unsafeIndexEndOfCodePoint'' to find the next codepoint.
  , backwardsIterChar :: {-# UNPACK #-} !CodePoint
    -- ^ The codepoint that we're focused on
  , backwardsIterEndOfChar :: {-# UNPACK #-} !CodeUnitIndex
    -- ^ Points to the last byte of the codepoint that we're focused on
  }

-- | Similar to unsafeIndexCodePoint', but assumes that the given index is the
-- end of a utf8 codepoint. It returns the decoded code point and the index
-- _before_ the code point. The resulting index could be passed directly to
-- unsafeIndexEndOfCodePoint' again to decode the _previous_ code point.
unsafeIndexEndOfCodePoint' :: TextArray.Array -> CodeUnitIndex -> BackwardsIter
{-# INLINE unsafeIndexEndOfCodePoint' #-}
unsafeIndexEndOfCodePoint' !u8data !idx =
  let
    cuAt !i = unsafeIndexCodeUnit' u8data $ idx - i
    -- Second, third and fourth bytes of a codepoint are always 10xxxxxx, while
    -- the first byte can be 0xxxxxxx or 11yyyyyy.
    isFirstByte !cu = cu .&. 0b1100_0000 /= 0b1000_0000
    cu0 = cuAt 0
  in
    if isFirstByte cu0
    then BackwardsIter (idx - 1) (decode1 cu0) idx
    else
      let cu1 = cuAt 1 in
      if isFirstByte cu1
      then BackwardsIter (idx - 2) (decode2 cu1 cu0) idx
      else
        let cu2 = cuAt 2 in
        if isFirstByte cu2
        then BackwardsIter (idx - 3) (decode3 cu2 cu1 cu0) idx
        else
          let cu3 = cuAt 3 in
          if isFirstByte cu3
          then BackwardsIter (idx - 4) (decode4 cu3 cu2 cu1 cu0) idx
          else
            error "unsafeIndexEndOfCodePoint' could not find valid UTF8 codepoint"

unsafeIndexAnywhereInCodePoint' :: TextArray.Array -> CodeUnitIndex -> BackwardsIter
{-# INLINE unsafeIndexAnywhereInCodePoint' #-}
unsafeIndexAnywhereInCodePoint' !u8data !idx =
  let
    cuAt !i = unsafeIndexCodeUnit' u8data $ idx + i
    -- Second, third and fourth bytes of a codepoint are always 10xxxxxx, while
    -- the first byte can be 0xxxxxxx or 11yyyyyy.
    isFirstByte !cu = cu .&. 0b1100_0000 /= 0b1000_0000
    cu0 = cuAt 0

    makeBackwardsIter next (l, cp) = BackwardsIter next cp (next + l)
  in
    if isFirstByte cu0
    then makeBackwardsIter (idx - 1) $ decodeN cu0 (cuAt 1) (cuAt 2) (cuAt 3)
    else
      let cu00 = cuAt (-1) in
      if isFirstByte cu00
      then makeBackwardsIter (idx - 2) $ decodeN cu00 cu0 (cuAt 1) (cuAt 2)
      else
        let cu000 = cuAt (-2) in
        if isFirstByte cu000
        then makeBackwardsIter (idx - 3) $ decodeN cu000 cu00 cu0 (cuAt 1)
        else
          let cu0000 = cuAt (-3) in
          if isFirstByte cu0000
          then makeBackwardsIter (idx - 4) $ decodeN cu0000 cu000 cu00 cu0
          else
            error "unsafeIndexAnywhereInCodePoint' could not find valid UTF8 codepoint"

{-# INLINE unsafeIndexCodeUnit' #-}
unsafeIndexCodeUnit' :: TextArray.Array -> CodeUnitIndex -> CodeUnit
unsafeIndexCodeUnit' !u8data (CodeUnitIndex !idx) = TextArray.unsafeIndex u8data idx

-- $generalFunctions
--
-- Re-exported from 'Text'.
