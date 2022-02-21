module Data.Text.Utf8.Utf8Spec where

import Control.Monad (forM_)
import qualified Data.Char as Char
import Test.Hspec (Spec, describe, it, shouldSatisfy)

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

-- | The Basic Multilingual Plane (BMP) contains the Unicode code points
-- 0x0000 through 0xFFFF.
bmpCodepoints :: [Int]
bmpCodepoints = [0..0x10000]

mapCp :: (Char -> Char) -> Int -> Int
mapCp f = Char.ord . f . Char.chr

commonSuffix :: Eq a => [a] -> [a] -> [a]
commonSuffix xs ys = reverse $ go (reverse xs) (reverse ys)
    where
        go (x:xs) (y:ys)
            | x == y = x : go xs ys
        go _ _ = []
