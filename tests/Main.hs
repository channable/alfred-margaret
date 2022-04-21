module Main where

import Test.Hspec (describe, hspec)

import Data.Text.AhoCorasickSpec as U8A
import Data.Text.BoyerMooreSpec as U8B
import Data.Text.Utf8Spec as U8

main :: IO ()
main = hspec $ do
  describe "Data.Text.AhoCorasick" U8A.spec
  describe "Data.Text.BoyerMoore" U8B.spec
  describe "Data.Text.Utf8" U8.spec
