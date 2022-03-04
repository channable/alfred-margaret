{-# LANGUAGE ScopedTypeVariables #-}
module Data.Text.Utf8.Utf8Spec where

import Control.Monad (forM_)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), choose)

import qualified Data.Char as Char

import Data.Text.Utf8 (stringToByteArray)

import qualified Data.Text.Utf8 as Utf8

spec :: Spec
spec = do
    describe "Properties of the BMP in UTF-8" $ do

        describe "Char.toLower" $ do

            {-
            it "does not generate common suffixes" $ do
                forM_ bmpCodepoints $ flip shouldSatisfy $ \cp ->
                    let
                        lowerCp = mapCp Char.toLower cp
                    in
                    cp == lowerCp || null (commonSuffix (Utf8.unicode2utf8 cp) (Utf8.unicode2utf8 lowerCp))
            -- Sadly, it "actually does"
            -}

            it "is idempotent" $ do
                forM_ bmpCodepoints $ flip shouldSatisfy $ \cp ->
                    mapCp Char.toLower cp == mapCp (Char.toLower . Char.toLower) cp

    describe "toLowerAscii" $ do

        it "is equivalent to Char.toLower on ASCII" $ do

            forM_ [0..0x7f] $ flip shouldSatisfy $ \cp ->
                mapCp Char.toLower cp == Utf8.toLowerAscii cp

    describe "lowerCodePoint" $ do

        prop "is equivalent to Char.toLower on all of Unicode" $ \c ->
            Utf8.lowerCodePoint (Char.ord c) `shouldBe` Char.ord (Char.toLower c)

    describe "slicing functions" $ do

        it "satisfies the example in Data.Text.Utf8" $ do
            let begin = Utf8.CodeUnitIndex 2
            let length_   = Utf8.CodeUnitIndex 6
            Utf8.unsafeSliceUtf8 begin length_ slicingExample `shouldBe` Utf8.pack "DEFGHI"
            Utf8.unsafeCutUtf8 begin length_ slicingExample `shouldBe` (Utf8.pack "BC", Utf8.pack "JKL")

        prop "unsafeSliceUtf8 and unsafeCutUtf8 are complementary" $ \ (SlicingExampleIndices begin length_ :: SlicingExampleIndices) -> do
            let (prefix, suffix) = Utf8.unsafeCutUtf8 begin length_ slicingExample
            Utf8.concat [prefix, Utf8.unsafeSliceUtf8 begin length_ slicingExample, suffix] `shouldBe` slicingExample


-- | Example shown in section "Slicing Functions" in 'Data.Text.Utf8".
slicingExample :: Utf8.Text
slicingExample = Utf8.Text (stringToByteArray "ABCDEFGHIJKLMN") 1 11

data SlicingExampleIndices = SlicingExampleIndices Utf8.CodeUnitIndex Utf8.CodeUnitIndex
    deriving Show

instance Arbitrary SlicingExampleIndices where
    arbitrary = do
        let exampleLength = unCodeUnitIndex $ Utf8.lengthUtf8 slicingExample

        begin <- choose (0, exampleLength)
        length_ <- choose (0, exampleLength - begin)

        pure $ SlicingExampleIndices (Utf8.CodeUnitIndex begin) (Utf8.CodeUnitIndex length_)

        where unCodeUnitIndex (Utf8.CodeUnitIndex i) = i

-- | The Basic Multilingual Plane (BMP) contains the Unicode code points
-- 0x0000 through 0xFFFF.
bmpCodepoints :: [Int]
bmpCodepoints = [0..0xffff]

mapCp :: (Char -> Char) -> Int -> Int
mapCp f = Char.ord . f . Char.chr

commonSuffix :: Eq a => [a] -> [a] -> [a]
commonSuffix list list' = reverse $ go (reverse list) (reverse list')
    where
        go (x:xs) (y:ys)
            | x == y = x : go xs ys
        go _ _ = []
