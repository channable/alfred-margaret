module Data.Text.Orphans where

import Test.QuickCheck (Arbitrary (..))

import qualified Test.QuickCheck.Gen as Gen

import Data.Text.Utf8 as Utf8

import Data.Text.CaseSensitivity (CaseSensitivity (..))

instance Arbitrary CaseSensitivity where
  arbitrary = Gen.elements [CaseSensitive, IgnoreCase]

-- TODO: Slow placeholder implementation until we can use text-2.0
instance Arbitrary Utf8.Text where
  arbitrary = fmap Utf8.pack arbitrary
