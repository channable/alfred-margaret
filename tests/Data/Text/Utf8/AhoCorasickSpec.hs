-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2022 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Text.Utf8.AhoCorasickSpec where

import Control.Monad (forM_)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Primitive (byteArrayFromList)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (modifyMaxSize, prop)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), forAll, forAllShrink)

import qualified Data.Text as T
import qualified Test.QuickCheck.Gen as Gen

import Data.Text.CaseSensitivity (CaseSensitivity (..))
import Data.Text.Orphans ()
import Data.Text.Utf8 (Text)

import qualified Data.Text.Utf8 as Text
import qualified Data.Text.Utf8 as Utf8
import qualified Data.Text.Utf8.AhoCorasick.Automaton as Aho
import qualified Data.Text.Utf8.AhoCorasick.Replacer as Replacer
import qualified Data.Text.Utf8.AhoCorasick.Searcher as Searcher
import qualified Data.Text.Utf8.AhoCorasick.Splitter as Splitter

spec :: Spec
spec = do
    -- Ensure that helper functions are actually helping
    -- Examples are from https://en.wikipedia.org/wiki/UTF-8
    describe "IsString ByteArray" $ do

        it "encodes the dollar sign" $ utf8Test "$" [0x24]
        it "encodes the euro sign" $ utf8Test "€" [0xe2, 0x82, 0xac]
        it "encodes the pound sign" $ utf8Test "£" [0xc2, 0xa3]
        it "encodes Hwair" $ utf8Test "𐍈" [0xf0, 0x90, 0x8d, 0x88]
        it "encodes all of the above" $ utf8Test "$€£𐍈" [0x24, 0xe2, 0x82, 0xac, 0xc2, 0xa3, 0xf0, 0x90, 0x8d, 0x88]

    describe "runText" $ do

        describe "countMatches" $ do
            it "counts the right number of matches in a basic example" $ do
                countMatches Aho.CaseSensitive ["abc", "rst", "xyz"] "abcdefghijklmnopqrstuvwxyz" `shouldBe` 3

            it "counts the right number of matches in an example with 1-, 2-, 3- and 4-code unit code points" $ do
                countMatches Aho.CaseSensitive ["$", "£"] "$€£𐍈" `shouldBe` 2

    describe "runLower" $ do

        describe "countMatches" $ do
            it "counts the right number of matches in a basic example" $ do
                countMatches Aho.IgnoreCase ["abc", "rst", "xyz"] "abcdefghijklmnopqrstuvwxyz" `shouldBe` 3

            it "does not work with uppercase needles" $ do
                countMatches Aho.IgnoreCase ["ABC", "Rst", "xYZ"] "abcdefghijklmnopqrstuvwxyz" `shouldBe` 0

            it "works with characters that are not in ASCII" $ do
                countMatches Aho.IgnoreCase ["groß", "öffnung", "tür"] "Großfräsmaschinenöffnungstür" `shouldBe` 3

    modifyMaxSize (const 10) $ describe "Replacer" $ do

        describe "run" $ do
            let
                genHaystack = fmap Utf8.pack $ Gen.listOf $ Gen.frequency [(40, Gen.elements "abAB"), (1, pure 'İ'), (1, arbitrary)]

                -- needles may not be empty, because empty needles are filtered out in an I.ActionReplaceMultiple
                genNeedle = fmap Utf8.pack $ Gen.resize 3 $ Gen.listOf1 $ Gen.elements "abAB"
                genReplaces = Gen.listOf $ (,) <$> genNeedle <*> arbitrary
                shrinkReplaces = filter (not . any (\(needle, _) -> Utf8.null needle)) . shrink

                replace needles haystack =
                    Replacer.run (Replacer.build Aho.CaseSensitive needles) haystack

                replaceIgnoreCase needles haystack =
                    Replacer.run (Replacer.build Aho.IgnoreCase needles) haystack

            it "replaces all occurrences" $ do
                replace [("A", "B")] "AXAXB" `shouldBe` "BXBXB"
                replace [("A", "B"), ("X", "Y")] "AXAXB" `shouldBe` "BYBYB"
                replace [("aaa", ""), ("b", "c")] "aaabaaa" `shouldBe` "c"
                -- Have a few non-matching needles too.
                replace [("A", "B"), ("Q", "r"), ("Z", "")] "AXAXB" `shouldBe` "BXBXB"

            it "replaces only non-overlapping matches" $ do
                replace [("aa", "zz"), ("bb", "w")] "aaabbb" `shouldBe` "zzawb"
                replace [("aaa", "")] "aaaaa" `shouldBe` "aa"

            it "replaces all occurrences in priority order" $ do
                replace [("A", ""), ("BBBB", "bingo")] "BBABB" `shouldBe` "bingo"
                replace [("BB", ""), ("BBBB", "bingo")] "BBBB" `shouldBe` ""

            it "replaces needles that contain a surrogate pair" $
                replace [("\x1f574", "levitating man in business suit")]
                    "the \x1f574" `shouldBe` "the levitating man in business suit"


            it "replaces all occurrences case-insensitively" $ do
                replaceIgnoreCase [("A", "B")] "AXAXB" `shouldBe` "BXBXB"
                replaceIgnoreCase [("A", "B")] "axaxb" `shouldBe` "BxBxb"
                replaceIgnoreCase [("a", "b")] "AXAXB" `shouldBe` "bXbXB"

                replaceIgnoreCase [("A", "B"), ("X", "Y")] "AXAXB" `shouldBe` "BYBYB"
                replaceIgnoreCase [("A", "B"), ("X", "Y")] "axaxb" `shouldBe` "BYBYb"
                replaceIgnoreCase [("a", "b"), ("x", "y")] "AXAXB" `shouldBe` "bybyB"

            it "matches replacements case-insensitively" $
              replaceIgnoreCase [("foo", "BAR"), ("bar", "BAZ")] "Foo" `shouldBe` "BAZ"

            it "matches replacements case-insensitively for non-ascii characters" $ do
                replaceIgnoreCase [("éclair", "lightning")] "Éclair" `shouldBe` "lightning"
                -- Note: U+0319 is an uppercase alpha, which looks exactly like A, but it
                -- is a different code point.
                replaceIgnoreCase [("bèta", "α"), ("\x0391", "alpha")] "BÈTA" `shouldBe` "alpha"

            it "matches surrogate pairs case-insensitively" $ do
                -- We can't lowercase a levivating man in business suit, but that should
                -- not affect whether we match it or not.
                replaceIgnoreCase [("\x1f574", "levitating man in business suit")] "the \x1f574"
                    `shouldBe` "the levitating man in business suit"

            prop "satisfies (run . compose a b) == (run b (run a))" $
                forAllShrink genHaystack shrink $ \haystack ->
                forAll arbitrary $ \case_ ->
                forAllShrink genReplaces shrinkReplaces $ \replaces1 ->
                forAllShrink genReplaces shrinkReplaces $ \replaces2 ->
                let
                    rm1 = Replacer.build case_ replaces1
                    rm2 = Replacer.build case_ replaces2
                    Just rm12 = Replacer.compose rm1 rm2
                in
                    Replacer.run rm2 (Replacer.run rm1 haystack)
                    `shouldBe` Replacer.run rm12 haystack

            prop "is identity for empty needles" $ \case_ haystack ->
                let replacerId = Replacer.build case_ []
                in Replacer.run replacerId haystack `shouldBe` haystack

            prop "is equivalent to sequential Text.replace calls" $
                forAllShrink genHaystack shrink $ \haystack ->
                forAllShrink genReplaces shrinkReplaces $ \replaces ->
                let
                    replacer = Replacer.build Aho.CaseSensitive replaces
                    -- TODO: Remove conversions once we move to text-2.0
                    replaceText agg (needle, replacement) = Utf8.pack $ T.unpack $ T.replace (T.pack $ Utf8.unpack needle) (T.pack $ Utf8.unpack replacement) (T.pack $ Utf8.unpack agg)
                    expected = foldl' replaceText haystack replaces
                in
                    Replacer.run replacer haystack `shouldBe` expected

    describe "Searcher" $ do

        describe "containsAny" $ do

            it "gives the right values for the examples in the README" $ do
                let needles = ["tshirt", "shirts", "shorts"]
                let searcher = Searcher.build Aho.CaseSensitive needles

                Searcher.containsAny searcher "short tshirts" `shouldBe` True
                Searcher.containsAny searcher "long shirt" `shouldBe` False
                Searcher.containsAny searcher "Short TSHIRTS" `shouldBe` False

                let searcher' = Searcher.build Aho.IgnoreCase needles

                Searcher.containsAny searcher' "Short TSHIRTS" `shouldBe` True

            it "works with the the first line of the illiad" $ do
                let illiad = "Ἄνδρα μοι ἔννεπε, Μοῦσα, πολύτροπον, ὃς μάλα πολλὰ"
                    needleSets = [(["μοι"], True), (["Ὀδυσεύς"], False)]

                forM_ needleSets $ \(needles, expectedResult) -> do
                    let searcher = Searcher.build Aho.CaseSensitive needles
                    Searcher.containsAny searcher illiad `shouldBe` expectedResult

        describe "containsAll" $ do

            prop "never reports true for empty needles" $ \ (haystack :: Text) ->
                let
                    searcher = Searcher.buildNeedleIdSearcher CaseSensitive [""]
                in
                    Searcher.containsAll searcher haystack `shouldBe` False

            prop "is equivalent to sequential Text.isInfixOf calls for non-empty needles" $ \ (needles' :: [NonEmptyText]) (haystack :: Text) ->
                let
                    needles = map unNonEmptyText needles'
                    searcher = Searcher.buildNeedleIdSearcher CaseSensitive needles
                in
                    Searcher.containsAll searcher haystack `shouldBe` all (`Text.isInfixOf` haystack) needles

            prop "is equivalent to sequential Text.isInfixOf calls for case-insensitive matching for non-empty needles" $ \ (needles' :: [NonEmptyText]) (haystack :: Text) ->
                let
                    needles = map unNonEmptyText needles'

                    lowerNeedles = map Utf8.lowerUtf8 needles
                    lowerHaystack = Utf8.lowerUtf8 haystack

                    searcher = Searcher.buildNeedleIdSearcher IgnoreCase lowerNeedles
                in
                    Searcher.containsAll searcher haystack `shouldBe` all (`Text.isInfixOf` lowerHaystack) lowerNeedles

    describe "Splitter" $ do

        describe "split" $ do

            it "passes an example" $ do
                let separator = "bob"
                    splitter = Splitter.build separator

                Splitter.split splitter "C++bobobCOBOLbobScala" `shouldBe` "C++" :| ["obCOBOL", "Scala"]

            it "neatly splits the first line of the illiad" $ do
                let splitter = Splitter.build ", "

                Splitter.split splitter "Ἄνδρα μοι ἔννεπε, Μοῦσα, πολύτροπον, ὃς μάλα πολλὰ" `shouldBe`
                    "Ἄνδρα μοι ἔννεπε" :| ["Μοῦσα", "πολύτροπον", "ὃς μάλα πολλὰ"]

-- helpers

utf8Test :: Utf8.Text -> [Utf8.CodeUnit] -> Expectation
utf8Test str byteList = str `shouldBe` Utf8.Text (byteArrayFromList byteList) 0 (length byteList)

-- From ./benchmark
countMatches :: Aho.CaseSensitivity -> [Utf8.Text] -> Utf8.Text -> Int
{-# NOINLINE countMatches #-}
countMatches caseSensitivity needles haystack = case needles of
  [] -> 0
  _  ->
    let
      ac = Aho.build $ zip needles (repeat ())
      onMatch !n _match = Aho.Step (n + 1)
    in
      Aho.runWithCase caseSensitivity 0 onMatch ac haystack

-- | A newtype for generating non-empty 'Text' values.
newtype NonEmptyText = NonEmptyText { unNonEmptyText :: Text }

-- | Simply generates and packs non-empty @[Char]@ values.
instance Arbitrary NonEmptyText where
    arbitrary = NonEmptyText . Text.pack <$> Gen.listOf1 arbitrary

instance Show NonEmptyText where
    show = show . unNonEmptyText
