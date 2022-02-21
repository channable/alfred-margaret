module Main where

import Control.Monad (forM)
import qualified Data.Text.Utf8 as Utf8
import Data.Text.Utf8.AhoCorasick.Automaton (debugBuildDot)
import System.Environment (getArgs)
import System.IO (hPrint, hPutStr, stderr)

main = do
    args <- getArgs
    needles <- forM args $ \needle -> do
        hPutStr stderr $ needle ++ ": "
        let needleBytes = Utf8.unpackUtf8 $ Utf8.pack needle
        hPrint stderr needleBytes
        pure needleBytes

    let dot = debugBuildDot needles
    putStrLn dot
