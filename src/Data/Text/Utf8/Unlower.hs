
module Data.Text.Utf8.Unlower
  (
    unlowerCodePoint

  , printUnlowerings
  ) where

import Control.Monad (forM_)

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List


-- | Inverse of Char.toLower/Utf8.lowerCodePoint
--
-- Returns all the characters that have the given character as their lower case, for example:
--
--    unlowerCodePoint 'a' == "aA"
--    unlowerCodePoint 'A' == ""
--    unlowerCodePoint '1' == "1"
--    unlowerCodePoint 'i' == "İiI"
--    unlowerCodePoint 'ß' == "ẞß"
--
unlowerCodePoint :: Char -> [Char]
unlowerCodePoint =
  \c -> maybe [c] id $ HashMap.lookup c unlowerings

-- | This map contains all the unlowerings for which the result is not just a singleton with the
-- input character. It's marked NOINLINE to make sure that it only gets constructed once.
unlowerings :: HashMap.HashMap Char [Char]
{-# NOINLINE unlowerings #-}
unlowerings =
  HashMap.filterWithKey isNotId $ List.foldl' (flip addUnlowering) initialMap [minBound..maxBound]
  where
    initialMap = HashMap.fromList $ zip [minBound..maxBound] (repeat [])
    addUnlowering c hm =
      HashMap.insertWith (++) (Char.toLower c) [c] hm
    isNotId lc ucs = ucs /= [lc]


-- | This function prints all the special cases of unlowerCodePoint where it's not @(pure . id)@:
--
--     SPECIAL: i (105) -> İ (304) i (105) I (73)
--     SPECIAL: k (107) -> K (8490) k (107) K (75)
--     SPECIAL: ß (223) -> ẞ (7838) ß (223)
--     SPECIAL: å (229) -> Å (8491) å (229) Å (197)
--     SPECIAL: ǆ (454) -> ǆ (454) ǅ (453) Ǆ (452)
--     SPECIAL: ǉ (457) -> ǉ (457) ǈ (456) Ǉ (455)
--     SPECIAL: ǌ (460) -> ǌ (460) ǋ (459) Ǌ (458)
--     SPECIAL: ǳ (499) -> ǳ (499) ǲ (498) Ǳ (497)
--     SPECIAL: θ (952) -> ϴ (1012) θ (952) Θ (920)
--     SPECIAL: ω (969) -> Ω (8486) ω (969) Ω (937)
--     [..]
--     Inverse of Char.toUpper: a (97) -> a (97) A (65)
--     Inverse of Char.toUpper: b (98) -> b (98) B (66)
--     Inverse of Char.toUpper: c (99) -> c (99) C (67)
--     [..]
--
printUnlowerings :: IO ()
printUnlowerings = do

  let
    showCP :: Char -> String
    showCP c = case Char.ord c of
      co | co > 68000 -> show co -- Some RTL languages above these code points are annoying to print
      co -> c : " (" <> show co <> ")"

    showCPs :: [Char] -> String
    showCPs cs = List.intercalate " " (map showCP cs)

    isInverse (lc, ucs) = ucs == [lc, Char.toUpper lc] || ucs == [Char.toUpper lc, lc]
    isAlreadyUppercase (_, ucs) = ucs == []
    isSpecial p = not (isInverse p) && not (isAlreadyUppercase p)

    lst :: [(Char, [Char])]
    lst = HashMap.toList unlowerings

  forM_ (filter isSpecial lst) $ \(lc, ucs) -> do
    putStrLn $ "SPECIAL: " <> showCP lc <> " -> " <> showCPs ucs

  forM_ (filter isAlreadyUppercase lst) $ \(lc, _) -> do
    putStrLn $ "Already uppercase (there is no unlowering): " <> showCP lc

  forM_ (filter isInverse lst) $ \(lc, ucs) -> do
    putStrLn $ "Inverse of Char.toUpper: " <> showCP lc <> " -> " <> List.intercalate " " (map showCP ucs)
