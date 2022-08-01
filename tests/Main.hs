module Main where

import Test.Hspec (describe, hspec)

import Data.Text.AhoCorasickSpec as AhoCorasickSpec
import Data.Text.BoyerMooreSpec as BoyerMooreSpec
import Data.Text.BoyerMooreCISpec as BoyerMooreCISpec
import Data.Text.Utf8Spec as Utf8Spec

main :: IO ()
main = hspec $ do
  describe "Data.Text.AhoCorasick" AhoCorasickSpec.spec
  describe "Data.Text.BoyerMoore" BoyerMooreSpec.spec
  describe "Data.Text.BoyerMooreCI" BoyerMooreCISpec.spec
  describe "Data.Text.Utf8" Utf8Spec.spec
