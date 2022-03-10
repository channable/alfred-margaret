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
{-# LANGUAGE MagicHash #-}

module Data.Text.Utf8
    ( CodePoint
    , CodeUnit
    , CodeUnitIndex (..)
    , Data.Text.Utf8.concat
    , Data.Text.Utf8.dropWhile
    , Data.Text.Utf8.null
    , Data.Text.Utf8.readFile
    , Data.Text.Utf8.replicate
    , Text (..)
    , decode2
    , decode3
    , decode4
    , decodeUtf8
    , indices
    , lengthUtf8
    , lowerCodePoint
    , lowerUtf8
    , pack
    , stringToByteArray
    , toLowerAscii
    , unicode2utf8
    , unpack
    , unpackUtf8
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
    , isInfixOf
    , unsafeCutUtf8
    , unsafeSliceUtf8
    ) where

import Control.DeepSeq (NFData, rnf)
import Data.Bits (Bits (shiftL), shiftR, (.&.), (.|.))
import Data.Char (ord)
import Data.Foldable (for_)
import Data.Hashable (Hashable (hashWithSalt), hashByteArrayWithSalt)
import Data.Primitive.ByteArray (ByteArray (ByteArray), byteArrayFromList, indexByteArray,
                                 newByteArray, sizeofByteArray, unsafeFreezeByteArray,
                                 writeByteArray)
import Data.Word (Word8)
import GHC.Base (Int (I#), compareByteArrays#)
import GHC.Generics (Generic)
import Prelude hiding (length)
#if defined(HAS_AESON)
import Data.Aeson (FromJSON, ToJSON, Value (String), parseJSON, toJSON, withText)
#endif

import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.Text as T

type CodeUnit = Word8
type CodePoint = Char

data Text
  -- | A placeholder data type for UTF-8 encoded text until we can use text-2.0.
  = Text
      !ByteArray -- ^ Underlying array encoded using UTF-8.
      !Int -- ^ Starting position of the UTF-8 sequence in bytes.
      !Int -- ^ Length of the UTF-8 sequence in bytes.
  deriving Show

-- This instance, as well as the Show instance above, is necessary for the test suite.
instance Eq Text where
  -- Since Data.Primitive.ByteArray.compareByteArrays is not available in lts-16.28, use the primitive version
  -- from GHC.Base
  (Text (ByteArray u8data) (I# offset) (I# length)) == (Text (ByteArray u8data') (I# offset') (I# length')) =
    I# length == I# length' && I# (compareByteArrays# u8data offset u8data' offset' length) == 0

-- Instances required for the Searcher modules etc.

#if defined(HAS_AESON)
-- NOTE: This is ugly and slow but will be removed once we move to text-2.0.
instance ToJSON Text where
  toJSON = String . T.pack . unpack

instance FromJSON Text where
  parseJSON = withText "Data.Text.Utf8.Text" (pure . pack . T.unpack)
#endif

-- NOTE: This is ugly and slow but will be removed once we move to text-2.0.
instance Ord Text where
  x <= y = unpack x <= unpack y

-- Copied from https://hackage.haskell.org/package/hashable-1.4.0.2/docs/src/Data.Hashable.Class.html#line-746
instance Hashable Text where
  hashWithSalt salt (Text (ByteArray arr) off len) =
    hashByteArrayWithSalt arr (off `shiftL` 1) (len `shiftL` 1) (hashWithSalt salt len)

instance NFData Text where
  rnf (Text (ByteArray !_) !_ !_) = ()

newtype CodeUnitIndex = CodeUnitIndex
    { codeUnitIndex :: Int
    }
    deriving stock (Eq, Ord, Show, Generic, Bounded)
#if defined(HAS_AESON)
    deriving newtype (Hashable, Num, NFData, AE.FromJSON, AE.ToJSON)
#else
    deriving newtype (Hashable, Num, NFData)
#endif

-- TODO: Slow placeholder implementation until we can use text-2.0
unpack :: Text -> String
unpack = decodeUtf8 . unpackUtf8

-- TODO: Slow placeholder implementation until we can use text-2.0
concat :: [Text] -> Text
concat = pack . concatMap unpack

null :: Text -> Bool
null (Text _ _ len) = len == 0

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

-- | Decode a list of UTF-8 code units into a list of code points.
decodeUtf8 :: [CodeUnit] -> [CodePoint]
decodeUtf8 [] = []
decodeUtf8 (cu0 : cus) | cu0 < 0xc0 = Char.chr (fromIntegral cu0) : decodeUtf8 cus
decodeUtf8 (cu0 : cu1 : cus) | cu0 < 0xe0 = Char.chr (decode2 cu0 cu1) : decodeUtf8 cus
decodeUtf8 (cu0 : cu1 : cu2 : cus) | cu0 < 0xf0 = Char.chr (decode3 cu0 cu1 cu2) : decodeUtf8 cus
decodeUtf8 (cu0 : cu1 : cu2 : cu3 : cus) | cu0 < 0xf8 = Char.chr (decode4 cu0 cu1 cu2 cu3) : decodeUtf8 cus
decodeUtf8 cus = error $ "Invalid UTF-8 input sequence at " ++ show (take 4 cus)

-- TODO: Slow placeholder implementation until we can use text-2.0
pack :: String -> Text
pack = go . stringToByteArray
  where
    go !arr = Text arr 0 $ sizeofByteArray arr

-- TODO: Slow placeholder implementation until we can use text-2.0
replicate :: Int -> Text -> Text
replicate n = pack . Prelude.concat . Prelude.replicate n . unpack

-- | Convert a 'Text' value into a 'T.Text' value.
toUtf16Text :: Text -> T.Text
toUtf16Text (Text u8data off len) =
  T.unfoldr go 0
  where
    go i
      | i >= len = Nothing
      | otherwise =
        let
          (codeUnits, codePoint) = unsafeIndexCodePoint' u8data (CodeUnitIndex $ off + i)
        in
          Just (codePoint, i + codeUnits)

-- TODO: Slow placeholder implementation until we can use text-2.0
isInfixOf :: Text -> Text -> Bool
isInfixOf needle haystack = T.isInfixOf (toUtf16Text needle) (toUtf16Text haystack)

-- TODO: Naive string search, replace once we have text-2.0
-- We can't use Data.Text.Internal.Search here because that one reports UTF-16 code unit indices.
-- Complexity: O(n*m)
indices :: Text -> Text -> [Int]
indices needle haystack
  | needleLen == 0 = []
  | otherwise = go 0 0
  where
    CodeUnitIndex needleLen = lengthUtf8 needle
    CodeUnitIndex haystackLen = lengthUtf8 haystack

    go startIdx needleIdx
      -- needle is longer than remaining haystack
      | startIdx + needleLen > haystackLen = []
      -- whole needle matched
      | needleIdx >= needleLen = startIdx : go (startIdx + needleLen) 0
      -- charachter mismatch
      | needleCp /= haystackCp = go (startIdx + 1) 0
      -- advance
      | otherwise = go startIdx $ needleIdx + codeUnits
      where
        (codeUnits, needleCp) = unsafeIndexCodePoint needle $ CodeUnitIndex needleIdx
        (_, haystackCp) = unsafeIndexCodePoint haystack $ CodeUnitIndex $ startIdx + needleIdx

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile predicate text =
  let
    len = codeUnitIndex (lengthUtf8 text)
    go i
      | i >= len = i
      | otherwise =
        let
          (codeUnits, codePoint) = unsafeIndexCodePoint text $ CodeUnitIndex i
        in
          if predicate codePoint then
            go $ i + codeUnits
          else
            i

    prefixEnd = go 0
  in
    unsafeSliceUtf8 (CodeUnitIndex prefixEnd) (CodeUnitIndex $ len - prefixEnd) text

stringToByteArray :: String -> ByteArray
stringToByteArray = byteArrayFromList . concatMap char2utf8
        -- See https://en.wikipedia.org/wiki/UTF-8
        where
            char2utf8 :: Char -> [Word8]
            char2utf8 = map fromIntegral . unicode2utf8 . ord

-- | Convert a Unicode Code Point 'c' into a list of UTF-8 code units (bytes).
unicode2utf8 :: (Ord a, Num a, Bits a) => a -> [a]
unicode2utf8 c
    | c < 0x80    = [c]
    | c < 0x800   = [0xc0 .|. (c `shiftR` 6), 0x80 .|. (0x3f .&. c)]
    | c < 0x10000 = [0xe0 .|. (c `shiftR` 12), 0x80 .|. (0x3f .&. (c `shiftR` 6)), 0x80 .|. (0x3f .&. c)]
    | otherwise   = [0xf0 .|. (c `shiftR` 18), 0x80 .|. (0x3f .&. (c `shiftR` 12)), 0x80 .|. (0x3f .&. (c `shiftR` 6)), 0x80 .|. (0x3f .&. c)]

readFile :: FilePath -> IO Text
readFile path = do
  contents <- BS.readFile path
  array <- newByteArray $ BS.length contents
  for_ [0..BS.length contents - 1] $ \i -> do
    writeByteArray array i $ BS.index contents i
  array' <- unsafeFreezeByteArray array
  pure $ Text array' 0 $ BS.length contents


-- | Decode 2 UTF-8 code units into their code point.
-- The given code units should have the following format: ->
--
-- > ┌───────────────┬───────────────┐
-- > │1 1 0 x x x x x│1 0 x x x x x x│
-- > └───────────────┴───────────────┘
{-# INLINE decode2 #-}
decode2 :: CodeUnit -> CodeUnit -> Int
decode2 cu0 cu1 =
  (fromIntegral cu0 .&. 0x1f) `shiftL` 6 .|. fromIntegral cu1 .&. 0x3f

-- | Decode 3 UTF-8 code units into their code point.
-- The given code units should have the following format:
--
-- > ┌───────────────┬───────────────┬───────────────┐
-- > │1 1 1 0 x x x x│1 0 x x x x x x│1 0 x x x x x x│
-- > └───────────────┴───────────────┴───────────────┘
{-# INLINE decode3 #-}
decode3 :: CodeUnit -> CodeUnit -> CodeUnit -> Int
decode3 cu0 cu1 cu2 =
  (fromIntegral cu0 .&. 0xf) `shiftL` 12 .|. (fromIntegral cu1 .&. 0x3f) `shiftL` 6 .|. (fromIntegral cu2 .&. 0x3f)

-- | Decode 4 UTF-8 code units into their code point.
-- The given code units should have the following format:
--
-- > ┌───────────────┬───────────────┬───────────────┬───────────────┐
-- > │1 1 1 1 0 x x x│1 0 x x x x x x│1 0 x x x x x x│1 0 x x x x x x│
-- > └───────────────┴───────────────┴───────────────┴───────────────┘
{-# INLINE decode4 #-}
decode4 :: CodeUnit -> CodeUnit -> CodeUnit -> CodeUnit -> Int
decode4 cu0 cu1 cu2 cu3 =
  (fromIntegral cu0 .&. 0x7) `shiftL` 18 .|. (fromIntegral cu1 .&. 0x3f) `shiftL` 12 .|. (fromIntegral cu2 .&. 0x3f) `shiftL` 6 .|. (fromIntegral cu3 .&. 0x3f)

-- | Lower-case the ASCII code points A-Z and leave the rest of ASCII intact.
{-# INLINE toLowerAscii #-}
toLowerAscii :: Char -> Char
toLowerAscii cp
  | Char.isAsciiUpper cp = Char.chr (Char.ord cp + 0x20)
  | otherwise = cp

-- TODO: Slow placeholder implementation until we can use text-2.0
{-# INLINE lowerUtf8 #-}
lowerUtf8 :: Text -> Text
lowerUtf8 = pack . map Char.toLower . unpack

asciiCount :: Int
asciiCount = 128

{-# INLINE lowerCodePoint #-}
-- | Lower-Case a UTF-8 codepoint.
-- Uses 'toLowerAscii' for ASCII and 'Char.toLower' otherwise.
lowerCodePoint :: Char -> Char
lowerCodePoint cp
  | Char.ord cp < asciiCount = toLowerAscii cp
  | otherwise = Char.toLower cp

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
unsafeIndexCodePoint' :: ByteArray -> CodeUnitIndex -> (Int, CodePoint)
unsafeIndexCodePoint' !u8data (CodeUnitIndex !idx)
  | cu0 < 0xc0 = (1, Char.chr $ fromIntegral cu0)
  | cu0 < 0xe0 = (2, Char.chr $ decode2 cu0 (cuAt 1))
  | cu0 < 0xf0 = (3, Char.chr $ decode3 cu0 (cuAt 1) (cuAt 2))
  | otherwise = (4, Char.chr $ decode4 cu0 (cuAt 1) (cuAt 2) (cuAt 3))
  where
    cuAt !i = unsafeIndexCodeUnit' u8data $ CodeUnitIndex $ idx + i
    !cu0 = cuAt 0

-- | Does exactly the same thing as 'unsafeIndexCodePoint'', but on 'Text' values.
{-# INLINE unsafeIndexCodePoint #-}
unsafeIndexCodePoint :: Text -> CodeUnitIndex -> (Int, CodePoint)
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
unsafeIndexCodeUnit' :: ByteArray -> CodeUnitIndex -> CodeUnit
unsafeIndexCodeUnit' !u8data (CodeUnitIndex !idx) = indexByteArray u8data idx

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

-- TODO: Make this more readable once we have text-2.0.
unsafeCutUtf8 :: CodeUnitIndex -- ^ Starting position of substring.
  -> CodeUnitIndex -- ^ Length of substring.
  -> Text -- ^ Initial string.
  -> (Text, Text)
unsafeCutUtf8 (CodeUnitIndex !begin) (CodeUnitIndex !length) (Text !u8data !off !len) =
  ( Text u8data off begin
  , Text u8data (off + begin + length) (len - begin - length)
  )

-- TODO: Make this more readable once we have text-2.0.
unsafeSliceUtf8 :: CodeUnitIndex -> CodeUnitIndex -> Text -> Text
unsafeSliceUtf8 (CodeUnitIndex !begin) (CodeUnitIndex !length) (Text !u8data !off !_len) =
  Text u8data (off + begin) length
