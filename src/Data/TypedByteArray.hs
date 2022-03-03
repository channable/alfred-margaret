-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2022 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

module Data.TypedByteArray
    ( TypedByteArray
    , fromList
    , unsafeIndex
    ) where

import Data.Primitive (ByteArray, Prim, byteArrayFromList, indexByteArray)

-- | Thin wrapper around 'ByteArray' that makes signatures and indexing nicer to read.
newtype TypedByteArray a = TypedByteArray ByteArray

fromList :: Prim a => [a] -> TypedByteArray a
fromList = TypedByteArray . byteArrayFromList

-- | Element index without bounds checking.
{-# INLINE unsafeIndex #-}
unsafeIndex :: Prim a => TypedByteArray a -> Int -> a
unsafeIndex (TypedByteArray arr) = indexByteArray arr
