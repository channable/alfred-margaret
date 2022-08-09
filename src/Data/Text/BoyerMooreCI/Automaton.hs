-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An efficient implementation of the Boyer-Moore string search algorithm.
-- http://www-igm.univ-mlv.fr/~lecroq/string/node14.html#SECTION00140
-- https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string-search_algorithm
--
-- This is case insensitive variant of the algorithm which, unlike the case
-- sensitive variant, has to be aware of the unicode code points that the bytes
-- represent.
--
module Data.Text.BoyerMooreCI.Automaton
    ( Automaton
    , CaseSensitivity (..)
    , CodeUnitIndex (..)
    , Next (..)
    , buildAutomaton
    , patternLength
    , patternText
    , runText

      -- Exposed for testing
    , minimumSkipForCodePoint
    ) where

import Control.DeepSeq (NFData)
import Control.Monad.ST (runST)
import Data.Hashable (Hashable (..))
import Data.Text.Internal (Text (..))
import GHC.Generics (Generic)

#if defined(HAS_AESON)
import qualified Data.Aeson as AE
#endif

import Data.Text.CaseSensitivity (CaseSensitivity (..))
import Data.Text.Utf8 (BackwardsIter (..), CodePoint, CodeUnitIndex (..))
import Data.TypedByteArray (Prim, TypedByteArray)

import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Utf8 as Utf8
import qualified Data.TypedByteArray as TBA

data Next a
  = Done !a
  | Step !a


-- | A Boyer-Moore automaton is based on lookup-tables that allow skipping through the haystack.
-- This allows for sub-linear matching in some cases, as we do not have to look at every input
-- character.
--
-- NOTE: Unlike the AcMachine, a Boyer-Moore automaton only returns non-overlapping matches.
-- This means that a Boyer-Moore automaton is not a 100% drop-in replacement for Aho-Corasick.
--
-- Returning overlapping matches would degrade the performance to /O(nm)/ in pathological cases like
-- finding @aaaa@ in @aaaaa....aaaaaa@ as for each match it would scan back the whole /m/ characters
-- of the pattern.
data Automaton = Automaton
  { automatonPattern :: !(TypedByteArray CodePoint)
  , automatonPatternHash :: !Int
  , automatonSuffixTable :: !SuffixTable
  , automatonBadCharLookup :: !BadCharLookup
  , automatonMinPatternSkip :: !CodeUnitIndex
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

instance Hashable Automaton where
  hashWithSalt salt = hashWithSalt salt . automatonPatternHash

instance Eq Automaton where
  x == y = automatonPattern x == automatonPattern y

#if defined(HAS_AESON)
instance AE.FromJSON Automaton where
  parseJSON v = buildAutomaton <$> AE.parseJSON v

instance AE.ToJSON Automaton where
  toJSON = AE.toJSON . patternText
#endif

buildAutomaton :: Text -> Automaton
buildAutomaton pattern_ =
  Automaton
    { automatonPattern = patternVec
    , automatonPatternHash = hash pattern_
    , automatonSuffixTable = buildSuffixTable patternVec
    , automatonBadCharLookup = buildBadCharLookup patternVec
    , automatonMinPatternSkip = minimumSkipForVector patternVec
    }
  where
    patternVec = TBA.fromList (Text.unpack pattern_)

-- | Finds all matches in the text, calling the match callback with the first and last byte index of
-- each match of the pattern.
runText  :: forall a
  . a
  -> (a -> CodeUnitIndex -> CodeUnitIndex -> Next a)
  -> Automaton
  -> Text
  -> a
{-# INLINE runText #-}
runText seed f automaton !text
  | TBA.null pattern_ = seed
  | otherwise = alignPattern seed initialHaystackMin (initialHaystackMin + minPatternSkip - 1)
  where
    Automaton pattern_ _ suffixTable badCharTable minPatternSkip = automaton

    -- In the pattern we always count codepoints,
    -- in the haystack we always count code units

    -- Highest index that we're allowed to use in the text
    haystackMax = case text of Text _ offset len -> CodeUnitIndex (offset + len - 1)

    -- How far we can look back in the text data is first limited by the text
    -- offset, and later by what we matched before.
    initialHaystackMin = case text of Text _ offset _ -> CodeUnitIndex offset

    -- This is our _outer_ loop, called when the pattern is moved
    alignPattern
      :: a
      -> CodeUnitIndex  -- Don't read before this point in the haystack
      -> CodeUnitIndex  -- End of pattern is aligned at this point in the haystack
      -> a
    {-# INLINE alignPattern #-}
    alignPattern !result !haystackMin !alignmentEnd
      | alignmentEnd > haystackMax = result
      | otherwise =
          let
            !iter = Utf8.unsafeIndexAnywhereInCodePoint' (case text of Text d _ _ -> d) alignmentEnd
            !patternIndex = TBA.length pattern_ - 1
            -- End of char may be somewhere different than where we started looking
            !alignmentEnd' = backwardsIterEndOfChar iter
          in
            matchLoop result haystackMin alignmentEnd' iter patternIndex

    -- The _inner_ loop, called for every pattern character back to front within a pattern alignment.
    matchLoop
      :: a
      -> CodeUnitIndex  -- haystackMin, don't read before this point in the haystack
      -> CodeUnitIndex  -- (adjusted) alignmentEnd, end of pattern is aligned at this point in the haystack
      -> BackwardsIter
      -> Int            -- index in the pattern
      -> a
    matchLoop !result !haystackMin !alignmentEnd !iter !patternIndex =
      let
        !haystackCodePointLower = Utf8.lowerCodePoint (backwardsIterChar iter)
      in
        case haystackCodePointLower == TBA.unsafeIndex pattern_ patternIndex of

          True | patternIndex == 0 ->
            -- We found a complete match (all pattern characters matched)
            let !from = backwardsIterNext iter + 1 - initialHaystackMin
                !to = alignmentEnd - initialHaystackMin
            in
              case f result from to of
                Done final -> final
                Step intermediate ->
                  let haystackMin' = alignmentEnd + 1  -- Disallow overlapping matches
                      alignmentEnd' = alignmentEnd + minPatternSkip
                  in alignPattern intermediate haystackMin' alignmentEnd'

          -- The pattern may be aligned in such a way that the start is before the start of the
          -- haystack. This _only_ happens when ⱥ and ⱦ characters occur (due to how minPatternSkip
          -- is calculated).
          True | backwardsIterNext iter < haystackMin ->
            alignPattern result haystackMin (alignmentEnd + 1)

          -- We continue by comparing the next character
          True ->
            let
              next = backwardsIterNext iter
              !iter' = Utf8.unsafeIndexEndOfCodePoint' (case text of Text d _ _ -> d) next
            in
              matchLoop result haystackMin alignmentEnd iter' (patternIndex - 1)

          -- Character did _not_ match at current position. Check how far the pattern has to move.
          False ->
            let
              -- The bad character table tells us how far we can advance to the right so that the
              -- character at the current position in the input string, where matching failed,
              -- is lined up with it's rightmost occurrence in the pattern.
              !fromBadChar =
                backwardsIterEndOfChar iter + badCharLookup badCharTable haystackCodePointLower

              -- This is always at least 1, ensuring that we make progress
              -- Suffixlookup tells us how far we can move the pattern
              !fromSuffixLookup =
                alignmentEnd + suffixLookup suffixTable patternIndex

              !alignmentEnd' = max fromBadChar fromSuffixLookup

            in
              -- Minimum stays the same
              alignPattern result haystackMin alignmentEnd'

-- | Length of the matched pattern measured in UTF-8 code units (bytes).
patternLength :: Automaton -> CodeUnitIndex
patternLength = Utf8.lengthUtf8 . patternText

-- | Return the pattern that was used to construct the automaton, O(n).
patternText :: Automaton -> Text
patternText = Text.pack . TBA.toList . automatonPattern


-- | Number of bytes that we can skip in the haystack if we want to skip no more
-- than 1 pattern codepoint.
--
-- It must always be a low (safe) estimate, otherwise the algorithm can miss
-- matches. It must account for any variation of upper/lower case characters
-- that may occur in the haystack. In most cases, this is the same number of
-- bytes as for the given codepoint
--
--     minimumSkipForCodePoint 'a' == 1
--     minimumSkipForCodePoint 'д' == 2
--     minimumSkipForCodePoint 'ⓟ' == 3
--     minimumSkipForCodePoint '🎄' == 4
--
minimumSkipForCodePoint :: CodePoint -> CodeUnitIndex
minimumSkipForCodePoint cp =
  case Char.ord cp of
    c | c < 0x80     -> 1
    c | c < 0x800    -> 2
    -- The letters ⱥ and ⱦ are 3 UTF8 bytes, but have unlowerings Ⱥ and Ⱦ of 2 bytes
    0x2C65           -> 2  -- ⱥ
    0x2C66           -> 2  -- ⱦ
    c | c < 0x10000  -> 3
    _                -> 4


-- | Number of bytes of the shortest case variation of the given needle. Needles
-- are assumed to be lower case.
--
--     minimumSkipForVector (TBA.fromList "ab..cd") == 6
--     minimumSkipForVector (TBA.fromList "aⱥ💩") == 7
--
minimumSkipForVector :: TypedByteArray CodePoint -> CodeUnitIndex
minimumSkipForVector = TBA.foldr (\cp s -> s + minimumSkipForCodePoint cp) 0


-- | The suffix table tells us for each codepoint (not byte!) of the pattern how many bytes (not
-- codepoints!) we can jump ahead if the match fails at that point.
newtype SuffixTable = SuffixTable (TypedByteArray CodeUnitIndex)
  deriving stock (Generic)
  deriving anyclass (NFData)

instance Show SuffixTable where
  show (SuffixTable table) = "SuffixTable (TBA.toList " <> show (TBA.toList table) <> ")"

-- | Lookup an entry in the suffix table.
suffixLookup :: SuffixTable -> Int -> CodeUnitIndex
{-# INLINE suffixLookup #-}
suffixLookup (SuffixTable table) = indexTable table

buildSuffixTable :: TypedByteArray CodePoint -> SuffixTable
buildSuffixTable pattern_ = runST $ do
  let
    patLen = TBA.length pattern_
    wholePatternSkip = minimumSkipForVector pattern_

  table <- TBA.newTypedByteArray patLen

  let
    -- Case 1: For each position of the pattern we record the shift that would align the pattern so
    -- that it starts at the longest suffix that is at the same time a prefix, if a mismatch would
    -- happen at that position.
    --
    -- Suppose the length of the pattern is n, a mismatch occurs at position i in the pattern and j
    -- in the haystack, then we know that pattern[i+1..n] == haystack[j+1..j+n-i]. That is, we know
    -- that the part of the haystack that we already matched is a suffix of the pattern.
    -- If the pattern happens to have a prefix that is equal to or a shorter suffix of that matched
    -- suffix, we can shift the pattern to the right so that the pattern starts at the longest
    -- suffix that we have seen that conincides with a prefix of the pattern.
    --
    -- Consider the pattern `ababa`. Then we get
    --
    -- p:              0  1  2  3  4
    -- Pattern:        a  b  a  b  a
    -- lastSkipBytes:              5   not touched by init1
    -- lastSkipBytes:           4  5   "a" == "a" so if we get a mismatch here we can skip
    --                                            everything but the length of "a"
    -- lastSkipBytes:        4  4  5   "ab" /= "ba" so keep skip value
    -- lastSkipBytes:     2  4  4  5   "aba" == "aba"
    -- lastSkipBytes:  2  2  4  4  5   "abab" /= "baba"
    init1 lastSkipBytes p
      | p >= 0 = do
        let
          skipBytes = case suffixIsPrefix pattern_ (p + 1) of
                        Nothing -> lastSkipBytes
                        -- Skip the whole pattern _except_ the bytes for the suffix(==prefix)
                        Just nonSkippableBytes -> wholePatternSkip - nonSkippableBytes
        TBA.writeTypedByteArray table p skipBytes
        init1 skipBytes (p - 1)
      | otherwise = pure ()

    -- Case 2: We also have to account for the fact that the matching suffix of the pattern might
    -- occur again somewhere within the pattern. In that case, we may not shift as far as if it was
    -- a prefix. That is why the `init2` loop is run after `init1`, potentially overwriting some
    -- entries with smaller shifts.
    init2 p skipBytes
      | p < patLen - 1 = do
          -- If we find a suffix that ends at p, we can skip everything _after_ p.
          let skipBytes' = skipBytes - minimumSkipForCodePoint (TBA.unsafeIndex pattern_ p)
          case substringIsSuffix pattern_ p of
            Nothing -> pure ()
            Just suffixLen -> do
              TBA.writeTypedByteArray table (patLen - 1 - suffixLen) skipBytes'
          init2 (p + 1) skipBytes'
      | otherwise = pure ()

  init1 (wholePatternSkip-1) (patLen - 1)
  init2 0 wholePatternSkip
  TBA.writeTypedByteArray table (patLen - 1) (CodeUnitIndex 1)

  SuffixTable <$> TBA.unsafeFreezeTypedByteArray table

-- | True if the suffix of the @pattern@ starting from @pos@ is a prefix of the pattern
-- For example, @suffixIsPrefix \"aabbaa\" 4 == Just 2@.
suffixIsPrefix :: TypedByteArray CodePoint -> Int -> Maybe CodeUnitIndex
suffixIsPrefix pattern_ pos = go 0 (CodeUnitIndex 0)
  where
    suffixLen = TBA.length pattern_ - pos
    go !i !skipBytes
      | i < suffixLen =
          let prefixChar = TBA.unsafeIndex pattern_ i in
          if prefixChar == TBA.unsafeIndex pattern_ (pos + i)
            then go (i + 1) (skipBytes + minimumSkipForCodePoint prefixChar)
            else Nothing
      | otherwise = Just skipBytes

-- | Length in bytes of the longest suffix of the pattern ending on @pos@.  For
-- example, @substringIsSuffix \"abaacbbaac\" 4 == Just 4@, because the
-- substring \"baac\" ends at position 4 and is at the same time the longest
-- suffix that does so, having a length of 4 characters.
--
-- For a string like "abaacaabcbaac", when we detect at pos=4 that baac==baac,
-- it means that if we get a mismatch before the "baac" suffix, we can skip the
-- "aabcbaac" characters _after_ the "baac" substring. So we can put
-- (minimumSkipForText "aabcbaac") at that point in the suffix table.
--
--   substringIsSuffix (Vector.fromList "ababa") 0 == Nothing  -- a == a, but not a proper substring
--   substringIsSuffix (Vector.fromList "ababa") 1 == Nothing  -- b /= a
--   substringIsSuffix (Vector.fromList "ababa") 2 == Nothing  -- aba == aba, but not a proper substring
--   substringIsSuffix (Vector.fromList "ababa") 3 == Nothing  -- b /= a
--   substringIsSuffix (Vector.fromList "ababa") 4 == Nothing  -- ababa == ababa, but not a proper substring
--   substringIsSuffix (Vector.fromList "baba") 0 == Nothing  -- b /= a
--   substringIsSuffix (Vector.fromList "baba") 1 == Nothing  -- ba == ba, but not a proper substring
--   substringIsSuffix (Vector.fromList "abaacaabcbaac") 4 == Just 4  -- baac == baac
--   substringIsSuffix (Vector.fromList "abaacaabcbaac") 8 == Just 1  -- c == c
--
substringIsSuffix :: TypedByteArray CodePoint -> Int -> Maybe Int
substringIsSuffix pattern_ pos = go 0
  where
    patLen = TBA.length pattern_
    go i | i > pos = Nothing  -- prefix==suffix, so already covered by suffixIsPrefix
         | TBA.unsafeIndex pattern_ (pos - i) == TBA.unsafeIndex pattern_ (patLen - 1 - i) =
             go (i + 1)
         | i == 0 = Nothing  -- Nothing matched
         | otherwise = Just i


-- | The bad char table tells us how many bytes we may skip ahead when encountering a certain
-- character in the input string. For example, if there's a character that is not contained in the
-- pattern at all, we can skip ahead until after that character.
data BadCharLookup = BadCharLookup
  { badCharLookupTable :: {-# UNPACK #-} !(TypedByteArray CodeUnitIndex)
  , badCharLookupMap :: !(HashMap.HashMap CodePoint CodeUnitIndex)
  , badCharLookupDefault :: !CodeUnitIndex
  }
  deriving stock (Generic, Show)
  deriving anyclass (NFData)

-- | Number of entries in the fixed-size lookup-table of the bad char table.
badCharTableSize :: Int
{-# INLINE badCharTableSize #-}
badCharTableSize = 256

-- | Lookup an entry in the bad char table.
badCharLookup :: BadCharLookup -> CodePoint -> CodeUnitIndex
{-# INLINE badCharLookup #-}
badCharLookup (BadCharLookup bclTable bclMap bclDefault) char
  | intChar < badCharTableSize = indexTable bclTable intChar
  | otherwise = HashMap.lookupDefault bclDefault char bclMap
  where
    intChar = fromEnum char



buildBadCharLookup :: TypedByteArray CodePoint -> BadCharLookup
buildBadCharLookup pattern_ = runST $ do

  let
    defaultSkip = minimumSkipForVector pattern_

  -- Initialize table with the maximum skip distance, which is the length of the pattern.
  -- This applies to all characters that are not part of the pattern.
  table <- (TBA.replicate badCharTableSize defaultSkip)

  let
    -- Fill the bad character table based on the rightmost occurrence of a character in the pattern.
    -- Note that there is also a variant of Boyer-Moore that records all positions (see Wikipedia,
    -- but that requires even more storage space).
    -- Also note that we exclude the last character of the pattern when building the table.
    -- This is because
    --
    -- 1. If the last character does not occur anywhere else in the pattern and we encounter it
    --    during a mismatch, we can advance the pattern to just after that character:
    --
    --    Haystack: aaadcdabcdbb
    --    Pattern:    abcd
    --
    --    In the above example, we would match `d` and `c`, but then fail because `d` != `b`.
    --    Since `d` only occurs at the very last position of the pattern, we can shift to
    --
    --    Haystack: aaadcdabcdbb
    --    Pattern:      abcd
    --
    -- 2. If it does occur anywhere else in the pattern, we can only shift as far as it's necessary
    --    to align it with the haystack:
    --
    --    Haystack: aaadddabcdbb
    --    Pattern:    adcd
    --
    --    We match `d`, and then there is a mismatch `d` != `c`, which allows us to shift only up to:

    --    Haystack: aaadddabcdbb
    --    Pattern:     adcd



    fillTable !badCharMap !skipBytes = \case
      [] -> pure badCharMap
      [_] -> pure badCharMap  -- The last pattern character doesn't count.
      (!patChar : !patChars) ->
        let skipBytes' = skipBytes - minimumSkipForCodePoint patChar in
        if fromEnum patChar < badCharTableSize
        then do
          TBA.writeTypedByteArray table (fromEnum patChar) skipBytes'
          fillTable badCharMap skipBytes' patChars
        else
          let badCharMap' = HashMap.insert patChar skipBytes' badCharMap
          in fillTable badCharMap' skipBytes' patChars

  badCharMap <- fillTable HashMap.empty defaultSkip (TBA.toList pattern_)

  tableFrozen <- TBA.unsafeFreezeTypedByteArray table

  pure BadCharLookup
    { badCharLookupTable = tableFrozen
    , badCharLookupMap = badCharMap
    , badCharLookupDefault = defaultSkip
    }


-- Helper functions for easily toggling the safety of this module

-- | Read from a lookup table at the specified index.
indexTable :: Prim a => TypedByteArray a -> Int -> a
{-# INLINE indexTable #-}
indexTable = TBA.unsafeIndex
