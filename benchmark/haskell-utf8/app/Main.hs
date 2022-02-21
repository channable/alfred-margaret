{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fllvm -O2 -optlo=-O3 -optlo=-tailcallelim #-}

-- | Benchmark for our Aho-Corasick implementation.
module Main where

import Control.Exception (evaluate)
import Data.Foldable (for_, traverse_)
import System.IO (hPrint, stderr, stdout)
import Text.Printf (hPrintf)

import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified System.Clock as Clock
import qualified System.Environment as Env

import Control.Monad (when)
import qualified Data.Text.Utf8 as Utf8
import qualified Data.Text.Utf8.AhoCorasick.Automaton as Aho

main :: IO ()
main = Env.getArgs >>= traverse_ processFile

processFile :: FilePath -> IO ()
processFile path = do
  -- Decode UTF-16 input files
  inputLines <- Text.lines . Encoding.decodeUtf16LE <$> BS.readFile path
  let (needles', haystackLines) = List.break Text.null inputLines
  -- Turn the haystack into a UTF-8 byte array using String as an intermediate representation
  -- Not the nicest solution but it works well enough until we convert the dataset into UTF-8.
  -- haystackLines also contains the empty line we breaked on, drop it using tail
  let !haystack = Utf8.pack $ Text.unpack $ Text.unlines $ tail haystackLines
  -- Turn each needle into a UTF-8 byte array.
  let !needles = map (Utf8.pack . Text.unpack) needles'

  -- ByteArray has no NFData instance :(
  -- void $ evaluate $ force needles
  -- void $ evaluate $ force haystack

  for_ [0 :: Int .. 5] $ \i -> do
    (count, duration) <- acBench needles haystack
    when (i == 0) $
      hPrint stderr count
    hPrintf stdout "%d\t" (Clock.toNanoSecs duration)
  hPrintf stdout "\n"

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
