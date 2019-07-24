module Data.Text.Orphans where

import Test.QuickCheck (Arbitrary (..))

import qualified Test.QuickCheck.Gen as Gen

import Data.Text.AhoCorasick.Automaton (CaseSensitivity (..))

instance Arbitrary CaseSensitivity where
  arbitrary = Gen.elements [CaseSensitive, IgnoreCase]
