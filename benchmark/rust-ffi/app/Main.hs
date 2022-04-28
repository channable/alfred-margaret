module Main where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Control.Monad (void, when)
import Data.Foldable (for_, traverse_)
import Data.Word (Word8)
import Foreign.C.Types (CBool (..), CSize (..))
import Foreign.Marshal (with, withArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (sizeOf), alignment, peek, poke, pokeByteOff, sizeOf)
import GHC.Compact (compact, getCompact)
import System.IO (hPrint, stderr, stdout)
import Text.Printf (hPrintf)

import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Encoding
import qualified System.Clock as Clock
import qualified System.Environment as Env

import Data.Text.Utf8 (CodeUnitIndex (..), Text (..))

import qualified Data.Text.Utf8 as Utf8

foreign import ccall unsafe "perform_ac"
  performAc :: CBool -> CSize -> Ptr U8Slice -> Ptr U8Slice -> IO CSize

-- | A slice of 'Word8's that can be passed to FFI.
-- The pointer should always point to pinned memory.
-- Use 'fromText' for constructing 'U8Slice's to ensure this.
data U8Slice = U8Slice (Ptr Word8) CSize CSize

instance Storable U8Slice where
  sizeOf _ = sizeOf (undefined :: Ptr Word8) + 2 * sizeOf (undefined :: CSize)
  alignment _ = max (alignment (undefined :: Ptr Word8)) (alignment (undefined :: CSize))
  peek _ptr = error "We only write U8Slices to pointer, never read them"
  poke ptr (U8Slice u8ptr off len) = do
    poke ptr' u8ptr
    pokeByteOff ptr' (sizeOf (undefined :: Ptr Word8)) off
    pokeByteOff ptr' (sizeOf (undefined :: Ptr Word8) + sizeOf (undefined :: CSize)) len
    where ptr' = castPtr ptr

-- | Turn a 'Text' value into something that can be passed through FFI.
-- The 'ByteArray' values inside must be pinned.
fromText :: Text -> U8Slice
fromText (Text u8data off len)
  | Utf8.isArrayPinned u8data = U8Slice (Utf8.arrayContents u8data) (fromIntegral off) (fromIntegral len)
  | otherwise                 = error "ByteArray is not pinned"

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
  (needles, haystack) <- getCompact <$> (compact =<< readNeedleHaystackFile path)

  void $ evaluate $ force needles
  void $ evaluate $ force haystack

  for_ [0 :: Int .. 4] $ \i -> do
    (count, duration) <- acBench needles haystack
    when (i == 0) $
      hPrint stderr count
    hPrintf stdout "%d\t" (Clock.toNanoSecs duration)
  hPrintf stdout "\n"

acBench :: [Text] -> Text -> IO (Int, Clock.TimeSpec)
acBench needles haystack = do
  start <- Clock.getTime Clock.Monotonic

  let numNeedles = fromIntegral $ length needles
  matchCount <- with (fromText haystack) $ \haystackSlice ->
    withArray (map fromText needles) $ \needleSlices -> do
      performAc useSparse numNeedles needleSlices haystackSlice

  end <- Clock.getTime Clock.Monotonic
  pure (fromIntegral matchCount, Clock.diffTimeSpec start end)
  where
    -- Whether to use the sparse or dense implementation.
    -- Set to @0@ to use the dense implementation.
    -- Set to @1@ to use the sparse implementation.
    useSparse :: CBool
    useSparse = 0
