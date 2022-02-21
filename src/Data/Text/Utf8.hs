-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2022 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE MagicHash #-}

module Data.Text.Utf8 (CodeUnit, CodeUnitIndex(..), Text(..), unpackUtf8, stringToByteArray, indexTextArray, unicode2utf8, pack) where

import Data.Bits (Bits, shiftR, (.&.), (.|.))
import Data.Primitive.ByteArray (ByteArray (ByteArray), byteArrayFromList, indexByteArray,
                                 sizeofByteArray)
import Data.Word (Word8)

import Data.Char (ord)
import GHC.Base (Int (I#), compareByteArrays#)
import Prelude hiding (length)

type CodeUnit = Word8

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

newtype CodeUnitIndex = CodeUnitIndex
    { codeUnitIndex :: Int
    }

{-# INLINABLE unpackUtf8 #-}
unpackUtf8 :: Text -> [CodeUnit]
unpackUtf8 (Text u8data offset length) =
  let
    go _ 0 = []
    go i n = indexTextArray u8data i : go (i + 1) (n - 1)
  in
    go offset length

{-# INLINE indexTextArray #-}
indexTextArray :: ByteArray -> Int -> CodeUnit
indexTextArray = indexByteArray

-- | Warning: This is slow placeholder function meant to be used for debugging.
pack :: String -> Text
pack = go . stringToByteArray
  where
    go arr = Text arr 0 $ sizeofByteArray arr

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
