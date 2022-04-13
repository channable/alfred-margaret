module Data.Text.Orphans where

import Test.QuickCheck (Arbitrary (..))

import qualified Test.QuickCheck.Gen as Gen

import Data.Text.CaseSensitivity (CaseSensitivity (..))
import Data.Text.Utf8

instance Arbitrary CaseSensitivity where
  arbitrary = Gen.elements [CaseSensitive, IgnoreCase]

instance Arbitrary CodeUnitIndex where
  arbitrary = fmap CodeUnitIndex arbitrary