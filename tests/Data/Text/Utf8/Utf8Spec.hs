{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Text.Utf8.Utf8Spec where

import Control.Monad (forM_)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary (arbitrary), choose)

import qualified Data.Char as Char

import Data.Text.Orphans ()
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
                    Char.toLower cp == Char.toLower (Char.toLower cp)

    describe "toLowerAscii" $ do

        it "is equivalent to Char.toLower on ASCII" $ do

            forM_ asciiCodepoints $ flip shouldSatisfy $ \cp ->
                Char.toLower cp == Utf8.toLowerAscii cp

    describe "lowerCodePoint" $ do

        prop "is equivalent to Char.toLower on all of Unicode" $ \c ->
            Utf8.lowerCodePoint c `shouldBe` Char.toLower c

    describe "dropWhile" $ do

        it "handles a simple example well" $ do
            Utf8.dropWhile (== 'b') "bbba" `shouldBe` "a"

    describe "slicing functions" $ do

        it "satisfies the example in Data.Text.Utf8" $ do
            let begin = Utf8.CodeUnitIndex 2
            let length_ = Utf8.CodeUnitIndex 6
            Utf8.unsafeSliceUtf8 begin length_ slicingExample `shouldBe` "DEFGHI"
            Utf8.unsafeCutUtf8 begin length_ slicingExample `shouldBe` ("BC", "JKL")

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

asciiCodepoints :: [Char]
asciiCodepoints = map Char.chr [0..0x7f]

-- | The Basic Multilingual Plane (BMP) contains the Unicode code points
-- 0x0000 through 0xFFFF.
bmpCodepoints :: [Char]
bmpCodepoints = map Char.chr [0..0xffff]

commonSuffix :: Eq a => [a] -> [a] -> [a]
commonSuffix list list' = reverse $ go (reverse list) (reverse list')
    where
        go (x:xs) (y:ys)
            | x == y = x : go xs ys
        go _ _ = []
