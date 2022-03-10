{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fllvm -O2 -optlo=-O3 -optlo=-tailcallelim #-}

-- | Benchmark for our Aho-Corasick implementation.
module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (void, when)
import Data.Foldable (for_, traverse_)
import System.IO (hPrint, stderr, stdout)
import Text.Printf (hPrintf)

import qualified System.Clock as Clock
import qualified System.Environment as Env

import Data.Text.Utf8 (CodeUnitIndex (CodeUnitIndex))

import qualified Data.Text.Utf8 as Utf8
import qualified Data.Text.Utf8.AhoCorasick.Automaton as Aho

main :: IO ()
main = Env.getArgs >>= traverse_ processFile

processFile :: FilePath -> IO ()
processFile path = do
  -- TODO: Revert once we have text-2.0
  (needles, haystack) <- readNeedleHaystackFile path

  void $ evaluate $ force needles
  void $ evaluate $ force haystack

  for_ [0 :: Int .. 5] $ \i -> do
    (count, duration) <- acBench needles haystack
    when (i == 0) $
      hPrint stderr count
    hPrintf stdout "%d\t" (Clock.toNanoSecs duration)
  hPrintf stdout "\n"

readNeedleHaystackFile :: FilePath -> IO ([Utf8.Text], Utf8.Text)
readNeedleHaystackFile path = do
  (Utf8.Text u8data off len) <- Utf8.readFile path
  pure $ go u8data off len []
  where
    go u8data off 0 needles = (reverse needles, Utf8.Text u8data off 0)
    go u8data off len needles
      -- "line starts with newline char" ==> empty line, emit haystack as slice of u8data
      | Utf8.unsafeIndexCodeUnit' u8data (CodeUnitIndex off) == 10 = (reverse needles, Utf8.Text u8data (off + 1) (len - 1))
      | otherwise = consumeNeedle u8data off len needles off

    consumeNeedle u8data off len needles needleStart
      -- Newline ==> emit needle as slice of u8data
      | Utf8.unsafeIndexCodeUnit' u8data (CodeUnitIndex off) == 10 = go u8data (off + 1) (len - 1) $ Utf8.Text u8data needleStart (off - needleStart) : needles
      | otherwise = consumeNeedle u8data (off + 1) (len - 1) needles needleStart

acBench :: [Utf8.Text] -> Utf8.Text -> IO (Int, Clock.TimeSpec)
{-# NOINLINE acBench #-}
acBench needles haystack = do
  start <- Clock.getTime Clock.Monotonic
  matchCount <- evaluate $ countMatches needles haystack
  end <- Clock.getTime Clock.Monotonic
  pure (matchCount, Clock.diffTimeSpec start end)

countMatches :: [Utf8.Text] -> Utf8.Text -> Int
{-# NOINLINE countMatches #-}
countMatches needles haystack = case needles of
  [] -> 0
  _  ->
    let
      ac = Aho.build $ zip (map Utf8.unpackUtf8 needles) (repeat ())
      onMatch !n _match = Aho.Step (n + 1)
    in
      Aho.runText 0 onMatch ac haystack
