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

import qualified System.Clock as Clock
import qualified System.Environment as Env

-- Conditional function definitions
#if defined(HAS_UTF8)
import Data.Text.Utf8 (CodeUnitIndex(..), Text (..))

import qualified Data.Text.Utf8 as Utf8
import qualified Data.Text.Utf8.AhoCorasick.Automaton as Aho

readNeedleHaystackFile path = do
  (Text u8data off len) <- Utf8.readFile path
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

unpackCodeUnits :: Text -> Text
unpackCodeUnits = id
#else
import Data.Text (Text)

import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text.Encoding as Encoding

import Data.Text.Utf16 (CodeUnit)

import qualified Data.Text as Text
import qualified Data.Text.AhoCorasick.Automaton as Aho
import qualified Data.Text.Utf16 as Utf16

readNeedleHaystackFile path = do
  inputLines <- Text.lines . Encoding.decodeUtf16LE <$> BS.readFile path
  let (needles, haystackLines) = List.break Text.null inputLines
  -- haystackLines also contains the empty line we breaked on
      haystack = Text.unlines $ tail haystackLines
  pure (needles, haystack)

unpackCodeUnits :: Text -> [CodeUnit]
unpackCodeUnits = Utf16.unpackUtf16
#endif

-- These functions need to be defined by the conditional blocks above.
readNeedleHaystackFile :: FilePath -> IO ([Text], Text)
-- unpackCodeUnits :: Text -> [CodeUnit]

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
      ac = Aho.build $ zip (fmap unpackCodeUnits needles) (repeat ())
      onMatch !n _match = Aho.Step (n + 1)
    in
      Aho.runText 0 onMatch ac haystack
