{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fllvm -O2 -optlo=-O3 -optlo=-tailcallelim #-}

-- | Benchmark for our Aho-Corasick implementation.
module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (void, when)
import Data.Foldable (for_, traverse_)
import Data.Text (Text)
import System.IO (hPrint, stderr, stdout)
import Text.Printf (hPrintf)

import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified System.Clock as Clock
import qualified System.Environment as Env

import qualified Data.Text.AhoCorasick.Automaton as Aho
import qualified Data.Text.Utf16 as Utf16

main :: IO ()
main = Env.getArgs >>= traverse_ processFile

processFile :: FilePath -> IO ()
processFile path = do
  inputLines <- Text.lines . Encoding.decodeUtf16LE <$> BS.readFile path
  let (needles, haystackLines) = List.break Text.null inputLines
      -- haystackLines also contains the empty line we breaked on
      haystack = Text.unlines $ tail haystackLines

  void $ evaluate $ force needles
  void $ evaluate $ force haystack

  for_ [0 :: Int .. 5] $ \i -> do
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
      ac = Aho.build $ zip (fmap Utf16.unpackUtf16 needles) (repeat ())
      onMatch !n _match = Aho.Step (n + 1)
    in
      Aho.runText 0 onMatch ac haystack
