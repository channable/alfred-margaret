-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2022 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

module Data.Text.Utf8 (CodeUnit, CodeUnitIndex, unpackUtf8) where

import           Data.Primitive.ByteArray (ByteArray, indexByteArray,
                                           sizeofByteArray)
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
