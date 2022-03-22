module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (void, when)
import Data.Foldable (for_, traverse_)
import Data.Word (Word8)
import Foreign.C.Types (CSize (..))
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr)
import GHC.Compact (compact, getCompact)
import System.IO (hPrint, stderr, stdout)
import Text.Printf (hPrintf)

import qualified Data.Primitive.ByteArray as BA
import qualified System.Clock as Clock
import qualified System.Environment as Env

import Data.Text.Utf8 (CodeUnitIndex (..), Text (..))

import qualified Data.Text.Utf8 as Utf8

foreign import ccall unsafe "perform_ac"
  performAc :: CSize -> Ptr (Ptr Word8) -> Ptr CSize -> Ptr CSize -> Ptr Word8 -> CSize -> CSize -> IO CSize

readNeedleHaystackFile :: FilePath -> IO ([Text], Text)
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

main :: IO ()
main = Env.getArgs >>= traverse_ processFile

processFile :: FilePath -> IO ()
processFile path = do
  (needles, haystack) <- getCompact <$> (compact =<< readNeedleHaystackFile path)

  void $ evaluate $ force needles
  void $ evaluate $ force haystack

  for_ [0 :: Int .. 5] $ \i -> do
    (count, duration) <- acBench needles haystack
    when (i == 0) $
      hPrint stderr count
    hPrintf stdout "%d\t" (Clock.toNanoSecs duration)
  hPrintf stdout "\n"

acBench :: [Text] -> Text -> IO (Int, Clock.TimeSpec)
acBench needles haystack = do
  start <- Clock.getTime Clock.Monotonic
  let Text haystackArr haystackOff haystackLen = haystack

  {-
  printPtr $ BA.byteArrayContents haystackArr
  dumpBytes (BA.byteArrayContents haystackArr) (fromIntegral haystackOff) (fromIntegral haystackLen)
  -}

  let numNeedles = length needles

  matchCount <- withArray [BA.byteArrayContents ptr | Text ptr _ _ <- needles] $ \buffers ->
    withArray [fromIntegral off | Text _ off _ <- needles] $ \offs ->
      withArray [fromIntegral len | Text _ _ len <- needles] $ \lens -> do
        performAc (fromIntegral numNeedles) buffers offs lens (BA.byteArrayContents haystackArr) (fromIntegral haystackOff) (fromIntegral haystackLen)

  end <- Clock.getTime Clock.Monotonic
  pure (fromIntegral matchCount, Clock.diffTimeSpec start end)
