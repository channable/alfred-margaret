{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad.ST (runST)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)
import Data.Foldable (for_)
import Text.Printf (printf)

import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Unboxed.Mutable as UMVector

import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import qualified Data.TypedByteArray as TBA
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = defaultMain
  [ bgroup "tba" $ mkReadBenchs readTba genTba [6, 7, 8]
  , bgroup "uvector" $ mkReadBenchs readUVector genUVector [6, 7, 8]
  ]

mkReadBenchs :: NFData a => (Int -> a -> Int) -> (Int -> a) -> [Int] -> [Benchmark]
mkReadBenchs readPat gen powers =
  [ bench (printf "%d reads" n) $ nf (readPat n) $ unsafePerformIO $ evaluate $ force (gen n)
  | n <- map (10^) powers
  ]

readTba :: Int -> TBA.TypedByteArray Int -> Int
readTba !n !arr = go 0
  where
    go !i
      | i >= n    = 42
      | otherwise = go $ TBA.unsafeIndex arr i

genTba :: Int -> TBA.TypedByteArray Int
genTba n = runST $ do
  mutArr <- TBA.newTypedByteArray n
  for_ [0 .. n-1] $ \i -> TBA.writeTypedByteArray mutArr i $ i + 1
  TBA.unsafeFreezeTypedByteArray mutArr

readUVector :: Int -> UVector.Vector Int -> Int
readUVector !n !arr = go 0
  where
    go !i
      | i >= n    = 42
      | otherwise = go $ UVector.unsafeIndex arr i

genUVector :: Int -> UVector.Vector Int
genUVector n = runST $ do
  mutArr <- UMVector.new $ 2 * n
  for_ [0 .. 2*n-1] $ \i -> UMVector.write mutArr i $ i + 1 - n
  UVector.slice n n <$> UVector.unsafeFreeze mutArr
