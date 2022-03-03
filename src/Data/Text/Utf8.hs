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
    , Data.Text.Utf8.null
    , Data.Text.Utf8.readFile
    , Text (..)
    , decode2
    , decode3
    , decode4
    , decodeUtf8
    , indexTextArray
    , lengthUtf8
    , lowerUtf8
    , pack
    , stringToByteArray
    , toLowerAscii
    , unicode2utf8
    , unpack
    , unpackUtf8
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
type CodePoint = Int

data Text
  -- | A placeholder constructor for UTF-8 encoded text until we can use text-2.0.
  -- First Int marks the starting point of the UTF-8 sequence in the array (in bytes).
  -- Second Int is the length of the UTF-8 sequence (in bytes).
  = Text !ByteArray !Int !Int
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
unpack = map Char.chr . decodeUtf8 . unpackUtf8

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
    go i n = indexTextArray u8data i : go (i + 1) (n - 1)
  in
    go offset length

lengthUtf8 :: Text -> CodeUnitIndex
lengthUtf8 (Text _ _ !length) = CodeUnitIndex length

-- TODO: Make this more readable once we have text-2.0.
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
--
-- Visualizes:
--
-- > unsafeCutUtf8 2 6 "BCDEFGHIJKL" == ("BC", "JKL")
unsafeCutUtf8 :: CodeUnitIndex -> CodeUnitIndex -> Text -> (Text, Text)
unsafeCutUtf8 (CodeUnitIndex !begin) (CodeUnitIndex !length) (Text !u8data !off !len) =
  ( Text u8data off begin
  , Text u8data (off + begin + length) (len - begin - length)
  )

-- TODO: Make this more readable once we have text-2.0.
unsafeSliceUtf8 :: CodeUnitIndex -> CodeUnitIndex -> Text -> Text
unsafeSliceUtf8 (CodeUnitIndex !begin) (CodeUnitIndex !length) (Text !u8data !off !len) =
  Text u8data (off + begin) length

-- | Decode a list of UTF-8 code units into a list of code points.
decodeUtf8 :: [CodeUnit] -> [CodePoint]
decodeUtf8 [] = []
decodeUtf8 (cu0 : cus) | cu0 < 0xc0 = fromIntegral cu0 : decodeUtf8 cus
decodeUtf8 (cu0 : cu1 : cus) | cu0 < 0xe0 = decode2 cu0 cu1 : decodeUtf8 cus
decodeUtf8 (cu0 : cu1 : cu2 : cus) | cu0 < 0xf0 = decode3 cu0 cu1 cu2 : decodeUtf8 cus
decodeUtf8 (cu0 : cu1 : cu2 : cu3 : cus) = decode4 cu0 cu1 cu2 cu3 : decodeUtf8 cus
decodeUtf8 cus = error $ "Invalid UTF-8 input sequence at " ++ show (take 4 cus)

{-# INLINE indexTextArray #-}
indexTextArray :: ByteArray -> Int -> CodeUnit
indexTextArray = indexByteArray

-- TODO: Slow placeholder implementation until we can use text-2.0
pack :: String -> Text
pack = go . stringToByteArray
  where
    go !arr = Text arr 0 $ sizeofByteArray arr

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
-- @
-- ┌───────────────┬───────────────┐
-- │1 1 0 x x x x x│1 0 x x x x x x│
-- └───────────────┴───────────────┘
-- @
{-# INLINE decode2 #-}
decode2 :: CodeUnit -> CodeUnit -> Int
decode2 cu0 cu1 =
  (fromIntegral cu0 .&. 0x1f) `shiftL` 6 .|. fromIntegral cu1 .&. 0x3f

-- | Decode 3 UTF-8 code units into their code point.
-- The given code units should have the following format:
--
-- @
-- ┌───────────────┬───────────────┬───────────────┐
-- │1 1 1 0 x x x x│1 0 x x x x x x│1 0 x x x x x x│
-- └───────────────┴───────────────┴───────────────┘
-- @
{-# INLINE decode3 #-}
decode3 :: CodeUnit -> CodeUnit -> CodeUnit -> Int
decode3 cu0 cu1 cu2 =
  (fromIntegral cu0 .&. 0xf) `shiftL` 12 .|. (fromIntegral cu1 .&. 0x3f) `shiftL` 6 .|. (fromIntegral cu2 .&. 0x3f)

-- | Decode 4 UTF-8 code units into their code point.
-- The given code units should have the following format:
--
-- @
-- ┌───────────────┬───────────────┬───────────────┬───────────────┐
-- │1 1 1 1 0 x x x│1 0 x x x x x x│1 0 x x x x x x│1 0 x x x x x x│
-- └───────────────┴───────────────┴───────────────┴───────────────┘
-- @
{-# INLINE decode4 #-}
decode4 :: CodeUnit -> CodeUnit -> CodeUnit -> CodeUnit -> Int
decode4 cu0 cu1 cu2 cu3 =
  (fromIntegral cu0 .&. 0x7) `shiftL` 18 .|. (fromIntegral cu1 .&. 0x3f) `shiftL` 12 .|. (fromIntegral cu2 .&. 0x3f) `shiftL` 6 .|. (fromIntegral cu3 .&. 0x3f)

-- | Lower-case the ASCII code points A-Z and leave the rest of ASCII intact.
{-# INLINE toLowerAscii #-}
toLowerAscii :: (Ord p, Num p) => p -> p
toLowerAscii cp
  | cp >= fromIntegral (Char.ord 'A') && cp <= fromIntegral (Char.ord 'Z') = cp + 0x20
  | otherwise = cp

-- TODO: Slow placeholder implementation until we can use text-2.0
{-# INLINE lowerUtf8 #-}
lowerUtf8 :: Text -> Text
lowerUtf8 = pack . map Char.toLower . unpack
