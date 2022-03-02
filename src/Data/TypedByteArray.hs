-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2022 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.
{-# LANGUAGE BangPatterns #-}

module Data.TypedByteArray
    ( Prim
    , TypedByteArray
    , fromList
    , unsafeIndex
    ) where

import Control.DeepSeq (NFData (rnf))
import Data.Primitive (ByteArray (ByteArray), Prim, byteArrayFromList, indexByteArray)

-- | Thin wrapper around 'ByteArray' that makes signatures and indexing nicer to read.
newtype TypedByteArray a = TypedByteArray ByteArray

instance NFData (TypedByteArray a) where
    rnf (TypedByteArray (ByteArray !_)) = ()

fromList :: Prim a => [a] -> TypedByteArray a
fromList = TypedByteArray . byteArrayFromList

-- | Element index without bounds checking.
{-# INLINE unsafeIndex #-}
unsafeIndex :: Prim a => TypedByteArray a -> Int -> a
unsafeIndex (TypedByteArray arr) = indexByteArray arr
