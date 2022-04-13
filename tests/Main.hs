module Main where

import Test.Hspec (describe, hspec)

import Data.Text.Utf8.AhoCorasickSpec as U8A
import Data.Text.Utf8.BoyerMooreSpec as U8B
import Data.Text.Utf8.Utf8Spec as U8

main :: IO ()
main = hspec $ do
  describe "Data.Text.Utf8.AhoCorasick" U8A.spec
  describe "Data.Text.Utf8.BoyerMoore" U8B.spec
  describe "Data.Text.Utf8" U8.spec
