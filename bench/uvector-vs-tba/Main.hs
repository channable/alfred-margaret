{-# LANGUAGE BangPatterns #-}

-- | Microbenchmark to measure the difference in 'TypedByteArray' and 'Data.Vector.Unboxed.Vector' indexing performance.
-- The later is a slice into a 'ByteArray' which means that every time you retrieve and element, an offset
-- needs to be added to your index.
--
-- To reproduce:
--
-- @
-- stack bench alfred-margaret:uvector-vs-tba --ba '--output uvector-vs-tba.html'
-- @
--
-- You can pass a greater @--time-limit@ (in the single quotes) to increase the number of iterations.
module Main where

import Control.Monad.ST (runST)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)
import Data.Foldable (for_)
import Text.Printf (printf)

import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Unboxed.Mutable as UMVector

import qualified Data.TypedByteArray as TBA

main :: IO ()
main = defaultMain
  [ bgroup "tba" $ mkReadBenchs readTba genTba [7, 8]
  , bgroup "uvector" $ mkReadBenchs readUVector genUVector [7, 8]
  ]

mkReadBenchs
  :: (Int -> a -> Int) -- ^ Function that reads from an array @n@ times.
  -> (Int -> a)        -- ^ Function that constructs an array of length @n@.
  -> [Int]             -- ^ Which powers of 10 to pass for @n@.
  -> [Benchmark]
mkReadBenchs readPattern gen powers =
  [ bench (printf "%d reads" n) $ nf (readPattern n) $ gen n
  | n <- map (10^) powers
  ]

readTba :: Int -> TBA.TypedByteArray Int -> Int
readTba !n !arr = go 0
  where
    go !i
      | i >= n    = 42
      | otherwise = go $ TBA.unsafeIndex arr i

readUVector :: Int -> UVector.Vector Int -> Int
readUVector !n !arr = go 0
  where
    go !i
      | i >= n    = 42
      | otherwise = go $ UVector.unsafeIndex arr i

-- NOTE: We should probably measure pseudo-random access time as well, e.g. by shuffing the generated arrays.

genTba :: Int -> TBA.TypedByteArray Int
genTba n = runST $ do
  mutArr <- TBA.newTypedByteArray n
  for_ [0 .. n-1] $ \i -> TBA.writeTypedByteArray mutArr i $ i + 1
  TBA.unsafeFreezeTypedByteArray mutArr

-- | Generate an unboxed vector @v@ such that @v[i] == i + 1@.
-- This function also makes sure that the @offset@ property of the generated vector
-- is not @0@ in order to avoid GHC optimizing that out.
genUVector :: Int -> UVector.Vector Int
genUVector n = runST $ do
  mutArr <- UMVector.new $ 1 + n
  for_ [0 .. n] $ \i -> UMVector.write mutArr i i
  UVector.slice 1 n <$> UVector.unsafeFreeze mutArr
