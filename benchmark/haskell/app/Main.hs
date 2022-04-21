{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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

import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import qualified System.Clock as Clock
import qualified System.Environment as Env

import Data.Text.Utf8 (CodeUnitIndex(..), Text (..))

import qualified Data.Text.Utf8 as Utf8
import qualified Data.Text.AhoCorasick.Automaton as Aho

readNeedleHaystackFile :: FilePath -> IO ([Text], Text)
readNeedleHaystackFile path = do
  (Text u8data off len) <- Encoding.decodeUtf8 <$> ByteString.readFile path
  pure $ go u8data off len []
  where
    go u8data off 0 needles = (reverse needles, Text u8data off 0)
    go u8data off len needles
      -- "line starts with newline char" ==> empty line, emit haystack as slice of u8data
      | Utf8.unsafeIndexCodeUnit' u8data (CodeUnitIndex off) == 10 = (reverse needles, Text u8data (off + 1) (len - 1))
      | otherwise = consumeNeedle u8data off len needles off

    consumeNeedle u8data off len needles needleStart
      -- Newline ==> emit needle as slice of u8data
      | Utf8.unsafeIndexCodeUnit' u8data (CodeUnitIndex off) == 10 = go u8data (off + 1) (len - 1) $ Text u8data needleStart (off - needleStart) : needles
      | otherwise = consumeNeedle u8data (off + 1) (len - 1) needles needleStart

main :: IO ()
main = Env.getArgs >>= traverse_ processFile

processFile :: FilePath -> IO ()
processFile path = do
  (needles, haystack) <- readNeedleHaystackFile path

  void $ evaluate $ force needles
  void $ evaluate $ force haystack

  for_ [0 :: Int .. 4] $ \i -> do
    (count, duration) <- acBench needles haystack
    when (i == 0) $
      hPrint stderr count
    hPrintf stdout "%d\t" (Clock.toNanoSecs duration)
  hPrintf stdout "\n"

acBench :: [Text] -> Text -> IO (Int, Clock.TimeSpec)
{-# NOINLINE acBench #-}
acBench needles haystack = do
  start <- Clock.getTime Clock.Monotonic
  matchCount <- evaluate $ countMatches needles haystack
  end <- Clock.getTime Clock.Monotonic
  pure (matchCount, Clock.diffTimeSpec start end)

countMatches :: [Text] -> Text -> Int
{-# NOINLINE countMatches #-}
countMatches needles haystack = case needles of
  [] -> 0
  _  ->
    let
      ac = Aho.build $ zip needles (repeat ())
      onMatch !n _match = Aho.Step (n + 1)
    in
      Aho.runText 0 onMatch ac haystack
