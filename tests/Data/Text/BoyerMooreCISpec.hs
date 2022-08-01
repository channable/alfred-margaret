
module Data.Text.BoyerMooreCISpec
  ( spec
  ) where


import Control.Monad (forM_)
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Data.Char as Char
import qualified Data.Text.Utf8 as Utf8

import qualified Data.Text.BoyerMooreCI.Automaton as BoyerMooreCI

spec :: Spec
spec = do

  describe "minimumSkipForCodePoint" $
    it "should match the reference implementation" $ do
      forM_ [minBound..maxBound] $ \c ->
        BoyerMooreCI.minimumSkipForCodePoint c `shouldBe` refMinimumSkipForCodePoint c


-- Reference implementation for BoyerMooreCI.minimumSkipForCodePoint
refMinimumSkipForCodePoint :: Utf8.CodePoint -> BoyerMooreCI.CodeUnitIndex
refMinimumSkipForCodePoint cp =
  let codePointLength = length . Utf8.unicode2utf8 . Char.ord in
  case Utf8.unlowerCodePoint cp of
    [] ->
      -- Input is upper case, so this is undefined behaviour but we match what the real
      -- implementation does:
      BoyerMooreCI.CodeUnitIndex $ codePointLength cp
    ucs -> BoyerMooreCI.CodeUnitIndex $ minimum $ map codePointLength ucs
