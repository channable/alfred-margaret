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
import Data.Primitive (ByteArray (ByteArray), Prim, alignment, byteArrayFromList, indexByteArray,
                       newByteArray, unsafeFreezeByteArray, writeByteArray)

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

-- | Construct a 'TypedByteArray' of the given length by applying the function to each index in @[0..n-1]@.
generate :: forall a. Prim a => Int -> (Int -> a) -> TypedByteArray a
generate !n f = runST $ do
    -- Allocate enough space for n correctly aligned elements
    arr <- newByteArray $ alignment (undefined :: a) * n
    intLoop 0 n $ \i -> writeByteArray arr i $ f i

    TypedByteArray <$> unsafeFreezeByteArray arr

intLoop :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
intLoop !i !n p
    | i >= n = pure ()
    | otherwise = do
        p i
        intLoop (i + 1) n p
