-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2022 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.TypedByteArray
    ( Prim
    , TypedByteArray
    , fromList
    , generate
    , unsafeIndex
    ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad.ST (runST)
import Data.Primitive (ByteArray (ByteArray), Prim, byteArrayFromList, indexByteArray, newByteArray,
                       sizeOf, unsafeFreezeByteArray, writeByteArray)

-- | Thin wrapper around 'ByteArray' that makes signatures and indexing nicer to read.
newtype TypedByteArray a = TypedByteArray ByteArray
    deriving Show

instance NFData (TypedByteArray a) where
    rnf (TypedByteArray (ByteArray !_)) = ()

fromList :: Prim a => [a] -> TypedByteArray a
fromList = TypedByteArray . byteArrayFromList

-- | Element index without bounds checking.
{-# INLINE unsafeIndex #-}
unsafeIndex :: Prim a => TypedByteArray a -> Int -> a
unsafeIndex (TypedByteArray arr) = indexByteArray arr

{-# INLINE generate #-}
-- | Construct a 'TypedByteArray' of the given length by applying the function to each index in @[0..n-1]@.
generate :: forall a. Prim a => Int -> (Int -> a) -> TypedByteArray a
generate !n f = runST $ do
    -- Allocate enough space for n elements of type a
    arr <- newByteArray $ sizeOf (undefined :: a) * n
    intLoop 0 n $ \i -> i `seq` writeByteArray arr i $ f i

    TypedByteArray <$> unsafeFreezeByteArray arr

{-# INLINE intLoop #-}
intLoop :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
intLoop !iStart !n p = go iStart
    where
        go !i
            | i >= n = pure ()
            | otherwise = do
                p i
                go (i + 1)
