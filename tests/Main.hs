module Main where

import Test.Hspec (describe, hspec)

import Data.Text.AhoCorasickSpec as A
import Data.Text.BoyerMooreSpec as B

main :: IO ()
main = hspec $ do
  describe "Data.Text.AhoCorasick" A.spec
  describe "Data.Text.BoyerMoore" B.spec
