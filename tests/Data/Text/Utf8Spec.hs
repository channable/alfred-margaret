{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Text.Utf8Spec where

import Control.Exception (evaluate)
import Control.Monad (forM_)
import Test.Hspec (Spec, anyErrorCall, describe, it, shouldBe, shouldSatisfy, shouldThrow)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, choose, forAllShrink, shrink)

import qualified Data.Char as Char

import Data.Text.TestInstances ()

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

    describe "unlowerCodePoint" $ do

      it "should return nothing if it's not a lower case of anything" $ do
        Utf8.unlowerCodePoint 'A' `shouldBe` ""
        Utf8.unlowerCodePoint 'ẞ' `shouldBe` ""

      it "should return itself if it doesn't have any casings" $ do
        Utf8.unlowerCodePoint '1' `shouldBe` "1"

      it "can return multiple values" $ do
        Utf8.unlowerCodePoint 'a' `shouldBe` "aA"
        Utf8.unlowerCodePoint 'ß' `shouldBe` "ẞß"
        Utf8.unlowerCodePoint 'i' `shouldBe` "İiI"


    describe "dropWhile" $ do

        it "handles a simple example well" $ do
            Utf8.dropWhile (== 'b') "bbba" `shouldBe` "a"

    describe "slicing functions" $ do

        let
            -- | Example shown in section "Slicing Functions" in 'Data.Text.Utf8".
            slicingExample :: Utf8.Text
            slicingExample = Utf8.Text u8data 1 11
                where Utf8.Text u8data _ _ = Utf8.pack "ABCDEFGHIJKLMN"

        it "satisfies the example in Data.Text.Utf8" $ do
            let begin = Utf8.CodeUnitIndex 2
            let length_ = Utf8.CodeUnitIndex 6
            Utf8.unsafeSliceUtf8 begin length_ slicingExample `shouldBe` "DEFGHI"
            Utf8.unsafeCutUtf8 begin length_ slicingExample `shouldBe` ("BC", "JKL")

        prop "unsafeSliceUtf8 and unsafeCutUtf8 are complementary" $
            forAllShrink (arbitrarySlicingIndices slicingExample) shrink $ \ (begin, length_) -> do
                let (prefix, suffix) = Utf8.unsafeCutUtf8 begin length_ slicingExample
                Utf8.concat [prefix, Utf8.unsafeSliceUtf8 begin length_ slicingExample, suffix] `shouldBe` slicingExample

    describe "Basic Text instances" $ do

        prop "Show Text behaves like Show String" $ \ (str :: String) -> do
            show (Utf8.pack str) `shouldBe` show str

        prop "Eq Text behaves like Eq String" $ \ (a :: String) (b :: String) -> do
            Utf8.pack a == Utf8.pack b `shouldBe` a == b

        prop "Ord Text behaves like Ord String" $ \ (a :: String) (b :: String) -> do
            compare (Utf8.pack a) (Utf8.pack b) `shouldBe` compare a b

    describe "skipCodePointsBackwards" $ do
      it "works with ascii" $ do
        Utf8.skipCodePointsBackwards "abcd" 3 0 `shouldBe` 3
        Utf8.skipCodePointsBackwards "abcd" 3 1 `shouldBe` 2
        Utf8.skipCodePointsBackwards "abcd" 3 2 `shouldBe` 1
        Utf8.skipCodePointsBackwards "abcd" 3 3 `shouldBe` 0

      it "moves to start of codepoint if you skip 0" $ do
        Utf8.skipCodePointsBackwards "💩💩" 0 0 `shouldBe` 0
        Utf8.skipCodePointsBackwards "💩💩" 1 0 `shouldBe` 0
        Utf8.skipCodePointsBackwards "💩💩" 2 0 `shouldBe` 0
        Utf8.skipCodePointsBackwards "💩💩" 3 0 `shouldBe` 0
        Utf8.skipCodePointsBackwards "💩💩" 4 0 `shouldBe` 4
        Utf8.skipCodePointsBackwards "💩💩" 5 0 `shouldBe` 4
        Utf8.skipCodePointsBackwards "💩💩" 6 0 `shouldBe` 4
        Utf8.skipCodePointsBackwards "💩💩" 7 0 `shouldBe` 4

      it "can skip 1 multi-byte codepoint" $ do
        Utf8.skipCodePointsBackwards "💩💩" 4 1 `shouldBe` 0
        Utf8.skipCodePointsBackwards "💩💩" 5 1 `shouldBe` 0
        Utf8.skipCodePointsBackwards "💩💩" 6 1 `shouldBe` 0
        Utf8.skipCodePointsBackwards "💩💩" 7 1 `shouldBe` 0

      it "throws errors when you read out of bounds" $ do
        evaluate (Utf8.skipCodePointsBackwards "💩💩" 8 0) `shouldThrow` anyErrorCall
        evaluate (Utf8.skipCodePointsBackwards "💩💩" 7 2) `shouldThrow` anyErrorCall



arbitrarySlicingIndices :: Utf8.Text -> Gen (Utf8.CodeUnitIndex, Utf8.CodeUnitIndex)
arbitrarySlicingIndices example = do
    let exampleLength = Utf8.codeUnitIndex $ Utf8.lengthUtf8 example

    begin <- choose (0, exampleLength)
    length_ <- choose (0, exampleLength - begin)

    pure (Utf8.CodeUnitIndex begin, Utf8.CodeUnitIndex length_)

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
