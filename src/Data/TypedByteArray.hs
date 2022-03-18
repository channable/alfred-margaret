-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2022 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.TypedByteArray
    ( Data.TypedByteArray.replicate
    , MutableTypedByteArray
    , Prim
    , TypedByteArray
    , fromList
    , generate
    , newTypedByteArray
    , unsafeFreezeTypedByteArray
    , unsafeIndex
    , writeTypedByteArray
    ) where

import Control.DeepSeq (NFData (rnf))
import Control.Monad.Primitive (PrimMonad (PrimState))
import Control.Monad.ST (runST)
import Data.Primitive (ByteArray (ByteArray), MutableByteArray, Prim, byteArrayFromList,
                       indexByteArray, newByteArray, sizeOf, unsafeFreezeByteArray, writeByteArray)

-- | Thin wrapper around 'ByteArray' that makes signatures and indexing nicer to read.
newtype TypedByteArray a = TypedByteArray ByteArray
    deriving Show

-- | Thin wrapper around 'MutableByteArray s' that makes signatures and indexing nicer to read.
newtype MutableTypedByteArray a s = MutableTypedByteArray (MutableByteArray s)

instance NFData (TypedByteArray a) where
    rnf (TypedByteArray (ByteArray !_)) = ()

{-# INLINE newTypedByteArray #-}
newTypedByteArray :: forall a m. (Prim a, PrimMonad m) => Int -> m (MutableTypedByteArray a (PrimState m))
newTypedByteArray = fmap MutableTypedByteArray . newByteArray . (* sizeOf (undefined :: a))

{-# INLINE fromList #-}
fromList :: Prim a => [a] -> TypedByteArray a
fromList = TypedByteArray . byteArrayFromList

-- | Element index without bounds checking.
{-# INLINE unsafeIndex #-}
unsafeIndex :: Prim a => TypedByteArray a -> Int -> a
unsafeIndex (TypedByteArray arr) = indexByteArray arr

{-# INLINE generate #-}
-- | Construct a 'TypedByteArray' of the given length by applying the function to each index in @[0..n-1]@.
generate :: Prim a => Int -> (Int -> a) -> TypedByteArray a
generate !n f = runST $ do
    -- Allocate enough space for n elements of type a
    arr <- newTypedByteArray n
    intLoop 0 n $ \i -> i `seq` writeTypedByteArray arr i $ f i

    unsafeFreezeTypedByteArray arr

replicate :: (Prim a, PrimMonad m) => Int -> a -> m (MutableTypedByteArray a (PrimState m))
replicate n value = do
    arr <- newTypedByteArray n
    intLoop 0 n $ \i -> i `seq` writeTypedByteArray arr i value
    pure arr

{-# INLINE writeTypedByteArray #-}
writeTypedByteArray :: (Prim a, PrimMonad m) => MutableTypedByteArray a (PrimState m) -> Int -> a -> m ()
writeTypedByteArray (MutableTypedByteArray array) = writeByteArray array

{-# INLINE unsafeFreezeTypedByteArray #-}
unsafeFreezeTypedByteArray :: PrimMonad m => MutableTypedByteArray a (PrimState m) -> m (TypedByteArray a)
unsafeFreezeTypedByteArray (MutableTypedByteArray array) = TypedByteArray <$> unsafeFreezeByteArray array

{-# INLINE intLoop #-}
intLoop :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
intLoop !iStart !n p = go iStart
    where
        go !i
            | i >= n = pure ()
            | otherwise = do
                p i
                go (i + 1)
