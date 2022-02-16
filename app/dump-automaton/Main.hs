module Main where

import System.Environment (getArgs)
import qualified Data.Text.Utf8                       as Utf8
import           Data.Text.Utf8.AhoCorasick.Automaton

main = do
    args <- getArgs
    let needles = map (Utf8.unpackUtf8 . Utf8.stringToByteArray) args
    let dot = debugBuildDot needles
    putStrLn dot