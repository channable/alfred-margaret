-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2022 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

module Data.Text.Utf8 (CodeUnit, CodeUnitIndex(..), unpackUtf8, stringToByteArray, indexTextArray) where


import           Data.Bits                (shiftR, (.&.), (.|.))
import           Data.Char                (ord)
import           Data.Primitive.ByteArray (ByteArray, byteArrayFromList,
                                           indexByteArray, sizeofByteArray)
import           Data.Word                (Word8)

type CodeUnit = Word8

newtype CodeUnitIndex = CodeUnitIndex
    { codeUnitIndex :: Int
    }

{-# INLINABLE unpackUtf8 #-}
unpackUtf8 :: ByteArray -> [CodeUnit]
unpackUtf8 u8data =
  let
    go _ 0 = []
    go i n = indexTextArray u8data i : go (i + 1) (n - 1)
  in
    go 0 $ sizeofByteArray u8data

{-# INLINE indexTextArray #-}
indexTextArray :: ByteArray -> Int -> CodeUnit
indexTextArray = indexByteArray

stringToByteArray :: String -> ByteArray
stringToByteArray = byteArrayFromList . concatMap char2utf8
        -- See https://en.wikipedia.org/wiki/UTF-8
        where
            char2utf8 :: Char -> [Word8]
            char2utf8 = map fromIntegral . unicode2utf8 . ord

            unicode2utf8 c
                | c < 0x80    = [c]
                | c < 0x800   = [0xc0 .|. (c `shiftR` 6), 0x80 .|. (0x3f .&. c)]
                | c < 0x10000 = [0xe0 .|. (c `shiftR` 12), 0x80 .|. (0x3f .&. (c `shiftR` 6)), 0x80 .|. (0x3f .&. c)]
                | otherwise   = [0xf0 .|. (c `shiftR` 18), 0x80 .|. (0x3f .&. (c `shiftR` 12)), 0x80 .|. (0x3f .&. (c `shiftR` 6)), 0x80 .|. (0x3f .&. c)]
