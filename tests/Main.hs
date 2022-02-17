module Main where

import           Test.Hspec                     (describe, hspec)

import           Data.Text.AhoCorasickSpec      as A
import           Data.Text.BoyerMooreSpec       as B
import           Data.Text.Utf8.AhoCorasickSpec as U8A
import           Data.Text.Utf8.Utf8Spec        as U8

main :: IO ()
main = hspec $ do
  describe "Data.Text.AhoCorasick" A.spec
  describe "Data.Text.BoyerMoore" B.spec
  describe "Data.Text.Utf8.AhoCorasick" U8A.spec
  describe "Data.Text.Utf8.Utf8" U8.spec
