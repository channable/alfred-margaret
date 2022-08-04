module Data.Text.TestInstances where

import Data.Text.Internal (Text (..))
import Test.QuickCheck (Arbitrary (..), Gen)

import qualified Data.Text as Text
import qualified Data.Text.Array as TextArray
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Gen as Gen

import Data.Text.CaseSensitivity (CaseSensitivity (..))
import qualified Data.Text.Utf8 as Utf8



instance Arbitrary CaseSensitivity where
  arbitrary = Gen.elements [CaseSensitive, IgnoreCase]

instance Arbitrary Utf8.CodeUnitIndex where
  arbitrary = fmap Utf8.CodeUnitIndex arbitrary



-- | Copy the text such that it has an arbitrary offset, but the represented
-- text stays the same.
arbitraryOffset :: Text -> Gen Text
arbitraryOffset (Text sourceData sourceOffset sourceLength) = do
  QuickCheck.Positive destOffset <- arbitrary
  let destData = TextArray.run $ do
        arr <- TextArray.new (destOffset + sourceLength)
        TextArray.copyI sourceLength arr destOffset sourceData sourceOffset
        pure arr
  pure $ Text destData destOffset sourceLength


-- | Generate random needles and haystacks, such that the needles have a
-- reasonable probability of occuring in the haystack, which would hardly be the
-- case if we just generated random texts for all of them.
--
-- We do this by first generating a set of fragments, and then building the
-- haystack and needles by combining these fragments. This way we also get a lot
-- of partial matches where part of a needle does occur in the haystack, but the
-- full needle does not, as well as needles with a shared prefix or suffix. This
-- should fully stress the possible transitions in the search algorithms.
--
arbitraryNeedleHaystack :: Gen (Text, Text)
arbitraryNeedleHaystack = do
  -- Generate a set of fragments, all within the same arbitrarily chosen alphabet
  alphabet <- arbitraryAlphabet
  fragments <- Gen.listOf1 $ Gen.resize 5 (arbitraryFragment alphabet)
  let
    genSmall = Gen.scale (`div` 3) $ Gen.listOf1 $ Gen.elements fragments
    genBig = Gen.scale (* 4) $ Gen.listOf1 $ Gen.elements fragments
  needle <- arbitraryOffset =<< fmap Text.concat genSmall
  haystack <- arbitraryOffset =<< fmap Text.concat genBig
  pure (needle, haystack)

arbitraryNeedlesHaystack :: Gen ([Text], Text)
arbitraryNeedlesHaystack = do
  -- Generate a set of fragments, all within the same arbitrarily chosen alphabet
  alphabet <- arbitraryAlphabet
  fragments <- Gen.listOf1 $ Gen.resize 5 (arbitraryFragment alphabet)
  let
    genSmall = Gen.scale (`div` 3) $ Gen.listOf1 $ Gen.elements fragments
    genBig = Gen.scale (* 4) $ Gen.listOf1 $ Gen.elements fragments
  needles <- Gen.listOf1 (arbitraryOffset =<< fmap Text.concat genSmall)
  haystack <- arbitraryOffset =<< fmap Text.concat genBig
  pure (needles, haystack)

arbitraryFragment :: [Char] -> Gen Text
arbitraryFragment alphabet =
  arbitraryOffset =<< Text.pack <$> Gen.listOf1 (Gen.elements alphabet)

arbitraryAlphabet :: Gen [Char]
arbitraryAlphabet =
  Gen.oneof [ pure simpleAlphabet
            , pure fancyAlphabet
            , randomAlphabet
            ]

  where

    simpleAlphabet = "abAB12"  -- Some ascii, include numbers so that not everything has upper/lower cases

    fancyAlphabet =
      concat
        [ "яЯ"  -- Cyrillic, two-byte characters
        , "åÅÅ" -- Å '\8491' and Å '\197' both have å '\229' as lower case
        ++ "𝄞💩"  -- Four byte characters \119070 and \128169
        ++ "ßẞ"   -- Note that ẞ '\7838' lower cases to ß '\223', but ß '\223' upper cases to ß '\223'
        ]

    randomAlphabet = sequence $ replicate 8 QuickCheck.arbitrary
