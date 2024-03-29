{-# LANGUAGE BangPatterns #-}

-- | Microbenchmark to measure the difference in 'TypedByteArray' and 'Data.Vector.Unboxed.Vector' indexing performance.
-- The latter is a slice into a 'ByteArray' which means that every time you retrieve and element, an offset
-- needs to be added to your index.
--
-- To reproduce:
--
-- @
-- stack bench alfred-margaret:uvector-vs-tba --ba '--output uvector-vs-tba.html'
-- @
--
-- You can pass a greater @--time-limit@ (in the single quotes) to increase the number of iterations.
--
-- NOTE: 'readUVector' and 'genUVector' are marked @NOINLINE@ to prevent GHC optimizing away the indexing addition.
-- 'readTba' and 'genTba' are marked @NOINLINE@ as well for fairness, altough it shouldn't make a difference there.
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

{-# NOINLINE readTba #-}
readTba :: Int -> TBA.TypedByteArray Int -> Int
readTba !n !arr = go 0
  where
    go !i
      | i >= n    = 42
      | otherwise = go $ TBA.unsafeIndex arr i

{-# NOINLINE readUVector #-}
readUVector :: Int -> UVector.Vector Int -> Int
readUVector !n !arr = go 0
  where
    go !i
      | i >= n    = 42
      | otherwise = go $ UVector.unsafeIndex arr i

-- NOTE: We should probably measure pseudo-random access time as well, e.g. by shuffling the generated arrays.

{-# NOINLINE genTba #-}
genTba :: Int -> TBA.TypedByteArray Int
genTba n = runST $ do
  mutArr <- TBA.newTypedByteArray n
  for_ [0 .. n-1] $ \i -> TBA.writeTypedByteArray mutArr i $ i + 1
  TBA.unsafeFreezeTypedByteArray mutArr

-- | Generate an unboxed vector @v@ such that @v[i] == i + 1@.
{-# NOINLINE genUVector #-}
genUVector :: Int -> UVector.Vector Int
genUVector n = runST $ do
  mutArr <- UMVector.new n
  for_ [0 .. n-1] $ \i -> UMVector.write mutArr i $ i + 1
  UVector.unsafeFreeze mutArr
