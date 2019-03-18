-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2019 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

-- Compile this module with LLVM, rather than with the default code generator.
-- LLVM produces about 20% faster code.
-- We pass -fignore-asserts to improve performance: we ran this code with
-- asserts enabled in production for two months, and in this time, the asserts
-- have not been violated.
{-# OPTIONS_GHC -fllvm -O2 -optlo=-O3 -optlo=-tailcallelim -fignore-asserts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An efficient implementation of the Aho-Corasick string matching algorithm.
-- See http://web.stanford.edu/class/archive/cs/cs166/cs166.1166/lectures/02/Small02.pdf
-- for a good explanation of the algorithm.
--
-- The memory layout of the automaton, and the function that steps it, were
-- optimized to the point where string matching compiles roughly to a loop over
-- the code units in the input text, that keeps track of the current state.
-- Lookup of the next state is either just an array index (for the root state),
-- or a linear scan through a small array (for non-root states). The pointer
-- chases that are common for traversing Haskell data structures have been
-- eliminated.
--
-- The construction of the automaton has not been optimized that much, because
-- construction time is usually negligible in comparison to matching time.
-- Therefore construction is a two-step process, where first we build the
-- automaton as int maps, which are convenient for incremental construction.
-- Afterwards we pack the automaton into unboxed vectors.
module Data.Text.AhoCorasick.Automaton
  ( AcMachine (..)
  , build
  , runText
  , runLower
  , debugBuildDot
  , CaseSensitivity (..)
  , CodeUnit
  , CodeUnitIndex (..)
  , Match (..)
  , lengthUtf16
  , lowerUtf16
  , unpackUtf16
  , unsafeCutUtf16
  , unsafeSliceUtf16
  )
  where

import Prelude hiding (length, lookup)

import Control.Exception (assert)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.IntMap.Strict (IntMap)
import Data.Primitive.ByteArray (ByteArray (..))
import Data.Text.Internal (Text (..))
import Data.Word (Word16, Word64)
import GHC.Exts (Int (..), Word (..), indexWord16Array#)
import GHC.Generics (Generic)

import qualified Data.Char as Char
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Unsafe as TextUnsafe
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Primitive as PVector

data CaseSensitivity
  = CaseSensitive
  | IgnoreCase
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable)

-- | A numbered state in the Aho-Corasick automaton.
type State = Int

-- | A code unit is a 16-bit integer from which UTF-16 encoded text is built up.
-- The `Text` type is represented as a UTF-16 string.
type CodeUnit = Word16

-- | A transition is a pair of (code unit, next state). The code unit is 16 bits,
-- and the state index is 32 bits. We pack these together as a manually unlifted
-- tuple, because an unboxed Vector of tuples is a tuple of vectors, but we want
-- the elements of the tuple to be adjacent in memory. (The Word64 still needs
-- to be unpacked in the places where it is used.) The code unit is stored in
-- the least significant 32 bits, with the special value 2^16 indicating a
-- wildcard; the "failure" transition. Bit 17 through 31 (starting from zero,
-- both bounds inclusive) are always 0.
--
--  Bit 63 (most significant)                 Bit 0 (least significant)
--  |                                                                 |
--  v                                                                 v
-- |<--       goto state         -->|<-- zeros   -->| |<--   input  -->|
-- |SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS|000000000000000|W|IIIIIIIIIIIIIIII|
--                                                   |
--                                                   Wildcard bit (bit 16)
--
type Transition = Word64

-- | An index into the raw UTF-16 data of a `Text`. This is not the code point
-- index as conventionally accepted by `Text`, so we wrap it to avoid confusing
-- the two. Incorrect index manipulation can lead to surrogate pairs being
-- sliced, so manipulate indices with care. This type is also used for lengths.
newtype CodeUnitIndex = CodeUnitIndex
  { codeUnitIndex :: Int
  }
  deriving stock (Eq, Generic, Show)
  deriving newtype (Hashable, Ord, Num, Bounded)

data Match v = Match
  { matchPos   :: {-# UNPACK #-} !CodeUnitIndex
  -- ^ The code unit index past the last code unit of the match. Note that this
  -- is not a code *point* (Haskell `Char`) index; a code point might be encoded
  -- as two code units.
  , matchValue :: v
  -- ^ The payload associated with the matched needle.
  } deriving (Show, Eq)

-- | An Aho-Corasick automaton.
data AcMachine v = AcMachine
  { machineValues :: !(Vector.Vector [v])
  -- ^ For every state, the values associated with its needles. If the state is
  -- not a match state, the list is empty.
  , machineTransitions :: !(Vector.Vector Transitions)
  -- ^ For every state, the index into `machineTransitions` where the transition
  -- list for that state starts.
  , machineRootAsciiTransitions :: !(UVector.Vector Transition)
  -- ^ A lookup table for transitions from the root state, an optimization to
  -- avoid having to walk all transitions, at the cost of using a bit of
  -- additional memory.
  } deriving (Generic)

-- | The wildcard value is 2^16, one more than the maximal 16-bit code unit.
wildcard :: Integral a => a
wildcard = 0x10000

-- | Extract the code unit from a transition. The special wildcard transition
-- will return 0.
transitionCodeUnit :: Transition -> CodeUnit
transitionCodeUnit t = fromIntegral (t .&. 0xffff)

-- | Extract the goto state from a transition.
transitionState :: Transition -> State
transitionState t = fromIntegral (t `shiftR` 32)

-- | Test if the transition is not for a specific code unit, but the wildcard
-- transition to take if nothing else matches.
transitionIsWildcard :: Transition -> Bool
transitionIsWildcard t = (t .&. wildcard) == wildcard

newTransition :: CodeUnit -> State -> Transition
newTransition input state =
  let
    input64 = fromIntegral input :: Word64
    state64 = fromIntegral state :: Word64
  in
    (state64 `shiftL` 32) .|. input64

newWildcardTransition :: State -> Transition
newWildcardTransition state =
  let
    state64 = fromIntegral state :: Word64
  in
    (state64 `shiftL` 32) .|. wildcard

newtype Transitions = Transitions (UVector.Vector Transition)

-- | Find the transition for the code unit by doing a linear scan over all
-- transitions. Returns `Right nextState` if a transition exists, or `Left
-- failState` in case we encountered the fallback transition. It is assumed
-- that the transitions vector is terminated by a wildcard transition.
lookup :: CodeUnit -> Transitions -> Either State State
lookup !input (Transitions ts) = go 0
  where
    go !i = case ts UVector.! i of
      t | transitionIsWildcard t        -> Left (transitionState t)
      t | transitionCodeUnit t == input -> Right (transitionState t)
      _ -> go (i + 1)

-- | Pack transitions for each state into one contiguous array. In order to find
-- the transitions for a specific state, we also produce a vector of start
-- indices. All transition lists are terminated by a wildcard transition, so
-- there is no need to record the length.
packTransitions :: [[Transition]] -> Vector.Vector Transitions
packTransitions = Vector.fromList . fmap (Transitions . UVector.fromList)

-- | Construct an Aho-Corasick automaton for the given needles.
-- Takes a list of code units rather than `Text`, to allow mapping the code
-- units before construction, for example to lowercase individual code points,
-- rather than doing proper case folding (which might change the number of code
-- units).
build :: [([CodeUnit], v)] -> AcMachine v
build needlesWithValues =
  let
    -- Construct the Aho-Corasick automaton using IntMaps, which are a suitable
    -- representation when building the automaton. We use int maps rather than
    -- hash maps to ensure that the iteration order is the same as that of a
    -- vector.
    (numStates, transitionMap, initialValueMap) = buildTransitionMap needlesWithValues
    fallbackMap = buildFallbackMap transitionMap
    valueMap = buildValueMap transitionMap fallbackMap initialValueMap

    -- Convert the map of transitions, and the map of fallback states, into a
    -- list of transition lists, where every transition list is terminated by
    -- a wildcard transition to the fallback state.
    prependTransition ts input state = newTransition (fromIntegral input) state : ts
    makeTransitions fallback ts = IntMap.foldlWithKey' prependTransition [newWildcardTransition fallback] ts
    transitionsList = zipWith makeTransitions (IntMap.elems fallbackMap) (IntMap.elems transitionMap)

    -- Pack the transition lists into one contiguous array, and build the lookup
    -- table for the transitions from the root state.
    transitions = packTransitions transitionsList
    rootTransitions = buildAsciiTransitionLookupTable $ transitionMap IntMap.! 0
    values = Vector.generate numStates (valueMap IntMap.!)
  in
    AcMachine values transitions rootTransitions

-- | Build the automaton, and format it as Graphviz Dot, for visual debugging.
debugBuildDot :: [[CodeUnit]] -> String
debugBuildDot needles =
  let
    (_numStates, transitionMap, initialValueMap) =
      buildTransitionMap $ zip needles ([0..] :: [Int])
    fallbackMap = buildFallbackMap transitionMap
    valueMap = buildValueMap transitionMap fallbackMap initialValueMap

    dotEdge extra state nextState =
      "  " ++ (show state) ++ " -> " ++ (show nextState) ++ " [" ++ extra ++ "];"

    dotFallbackEdge :: [String] -> State -> State -> [String]
    dotFallbackEdge edges state nextState =
      (dotEdge "style = dashed" state nextState) : edges

    dotTransitionEdge :: State -> [String] -> Int -> State -> [String]
    dotTransitionEdge state edges input nextState =
      (dotEdge ("label = \"" ++ show input ++ "\"") state nextState) : edges

    prependTransitionEdges edges state =
      IntMap.foldlWithKey' (dotTransitionEdge state) edges (transitionMap IntMap.! state)

    dotMatchState :: [String] -> State -> [Int] -> [String]
    dotMatchState edges _ [] = edges
    dotMatchState edges state _ = ("  " ++ show state ++ " [shape = doublecircle];") : edges

    dot0 = foldBreadthFirst prependTransitionEdges [] transitionMap
    dot1 = IntMap.foldlWithKey' dotFallbackEdge dot0 fallbackMap
    dot2 = IntMap.foldlWithKey' dotMatchState dot1 valueMap
  in
    -- Set rankdir = "LR" to prefer a left-to-right graph, rather than top to
    -- bottom. I have dual widescreen monitors and I don't use them in portrait
    -- mode. Reverse the instructions because order affects node lay-out, and by
    -- prepending we built up a reversed list.
    unlines $ ["digraph {", "  rankdir = \"LR\";"] ++ (reverse dot2) ++ ["}"]

-- Different int maps that are used during constuction of the automaton. The
-- transition map represents the trie of states, the fallback map contains the
-- fallback (or "failure" or "suffix") edge for every state.
type TransitionMap = IntMap (IntMap State)
type FallbackMap = IntMap State
type ValuesMap v = IntMap [v]

-- | Build the trie of the Aho-Corasick state machine for all input needles.
buildTransitionMap :: forall v. [([CodeUnit], v)] -> (Int, TransitionMap, ValuesMap v)
buildTransitionMap =
  let
    go :: State
      -> (Int, TransitionMap, ValuesMap v)
      -> ([CodeUnit], v)
      -> (Int, TransitionMap, ValuesMap v)

    -- End of the current needle, insert the associated payload value.
    -- If a needle occurs multiple times, then at this point we will merge
    -- their payload values, so the needle is reported twice, possibly with
    -- different payload values.
    go !state (!numStates, transitions, values) ([], v) =
      (numStates, transitions, IntMap.insertWith (++) state [v] values)

    -- Follow the edge for the given input from the current state, creating it
    -- if it does not exist.
    go !state (!numStates, transitions, values) (!input : needleTail, vs) =
      let
        transitionsFromState = transitions IntMap.! state
      in
        case IntMap.lookup (fromIntegral input) transitionsFromState of
          Just nextState ->
            go nextState (numStates, transitions, values) (needleTail, vs)
          Nothing ->
            let
              -- Allocate a new state, and insert a transition to it.
              -- Also insert an empty transition map for it.
              nextState = numStates
              transitionsFromState' = IntMap.insert (fromIntegral input) nextState transitionsFromState
              transitions'
                = IntMap.insert state transitionsFromState'
                $ IntMap.insert nextState IntMap.empty
                $ transitions
            in
              go nextState (numStates + 1, transitions', values) (needleTail, vs)

    -- Initially, the root state (state 0) exists, and it has no transitions
    -- to anywhere.
    initialTransitions = IntMap.singleton stateInitial IntMap.empty
    initialValues = IntMap.empty
    insertNeedle = go stateInitial
  in
    foldl' insertNeedle (1, initialTransitions, initialValues)

-- Size of the ascii transition lookup table.
asciiCount :: Integral a => a
asciiCount = 128

-- | Build a lookup table for the first 128 code units, that can be used for
-- O(1) lookup of a transition, rather than doing a linear scan over all
-- transitions. The fallback goes back to the initial state, state 0.
buildAsciiTransitionLookupTable :: IntMap State -> UVector.Vector Transition
buildAsciiTransitionLookupTable transitions = UVector.generate asciiCount $ \i ->
  case IntMap.lookup i transitions of
    Just state -> newTransition (fromIntegral i) state
    Nothing -> newWildcardTransition 0

-- | Traverse the state trie in breadth-first order.
foldBreadthFirst :: (a -> State -> a) -> a -> TransitionMap -> a
foldBreadthFirst f seed transitions = go [0] [] seed
  where
    -- For the traversal, we keep a queue of states to vitit. Every iteration we
    -- take one off the front, and all states reachable from there get added to
    -- the back. Rather than using a list for this, we use the functional
    -- amortized queue to avoid O(n²) append. This makes a measurable difference
    -- when the backlog can grow large. In one of our benchmark inputs for
    -- example, we have roughly 160 needles that are 10 characters each (but
    -- with some shared prefixes), and the backlog size grows to 148 during
    -- construction. Construction time goes down from ~0.80 ms to ~0.35 ms by
    -- using the amortized queue.
    -- See also section 3.1.1 of Purely Functional Data Structures by Okasaki
    -- https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf.
    go [] [] !acc = acc
    go [] revBacklog !acc = go (reverse revBacklog) [] acc
    go (state : backlog) revBacklog !acc =
      let
        -- Note that the backlog never contains duplicates, because we traverse
        -- a trie that only branches out. For every state, there is only one
        -- path from the root that leads to it.
        extra = IntMap.elems $ transitions IntMap.! state
      in
        go backlog (extra ++ revBacklog) (f acc state)

-- | Determine the fallback transition for every state, by traversing the
-- transition trie breadth-first.
buildFallbackMap :: TransitionMap -> FallbackMap
buildFallbackMap transitions =
  let
    -- Suppose that in state `state`, there is a transition for input `input`
    -- to state `nextState`, and we already know the fallback for `state`. Then
    -- this function returns the fallback state for `nextState`.
    getFallback :: FallbackMap -> State -> Int -> State
    -- All the states after the root state (state 0) fall back to the root state.
    getFallback _ 0 _ = 0
    getFallback fallbacks !state !input =
      let
        fallback = fallbacks IntMap.! state
        transitionsFromFallback = transitions IntMap.! fallback
      in
        case IntMap.lookup input transitionsFromFallback of
          Just st -> st
          Nothing -> getFallback fallbacks fallback input

    insertFallback :: State -> FallbackMap -> Int -> State -> FallbackMap
    insertFallback !state fallbacks !input !nextState =
      IntMap.insert nextState (getFallback fallbacks state input) fallbacks

    insertFallbacks :: FallbackMap -> State -> FallbackMap
    insertFallbacks fallbacks !state =
      IntMap.foldlWithKey' (insertFallback state) fallbacks (transitions IntMap.! state)
  in
    foldBreadthFirst insertFallbacks (IntMap.singleton 0 0) transitions

-- | Determine which matches to report at every state, by traversing the
-- transition trie breadth-first, and appending all the matches from a fallback
-- state to the matches for the current state.
buildValueMap :: forall v. TransitionMap -> FallbackMap -> ValuesMap v -> ValuesMap v
buildValueMap transitions fallbacks valuesInitial =
  let
    insertValues :: ValuesMap v -> State -> ValuesMap v
    insertValues values !state =
      let
        fallbackValues = values IntMap.! (fallbacks IntMap.! state)
        valuesForState = case IntMap.lookup state valuesInitial of
          Just vs -> vs ++ fallbackValues
          Nothing -> fallbackValues
      in
        IntMap.insert state valuesForState values
  in
    foldBreadthFirst insertValues (IntMap.singleton 0 []) transitions

stateInitial :: State
stateInitial = 0

-- | Follow the edge for the input code unit from the current state. If no such
-- edge exists, follow the "failure" edge, and try again.
followEdge :: Vector.Vector Transitions -> CodeUnit -> State -> State
followEdge transitionsFrom !input !state =
  case lookup input (transitionsFrom Vector.! (fromIntegral state)) of
    -- Note: recursion halts eventually, because the failure transitions are
    -- not cyclic. Eventually we get back to the initial state, and there we
    -- stop the recursion.
    Right nextState -> nextState
    Left failState -> if state == stateInitial
      then state
      else followEdge transitionsFrom input failState

step
  :: AcMachine v
  -> TextArray.Array
  -> State
  -> Int
  -> Int
  -> [Match v]
  -> [Match v]
step machine u16data !state !offset !length !matches =
  let
    !(TextArray.Array bytes) = u16data
    !(I# i) = offset
    unitWord = indexWord16Array# bytes i
    unit16 = fromIntegral (W# unitWord)
    nextState = followEdge (machineTransitions machine) unit16 state
    values = (machineValues machine) Vector.! (fromIntegral nextState)
    prependMatch ms v = (Match (CodeUnitIndex offset) v) : ms
    matches' = foldl' prependMatch matches values
  in
    if length == 0
      then matches
      else step machine u16data nextState (offset + 1) (length - 1) matches'

runText :: AcMachine v -> Text -> [Match v]
runText machine (Text u16data offset length) =
  step machine u16data stateInitial offset length []

-- Finds all matches in the lowercased text. This function lowercases the text
-- on the fly to avoid allocating a second lowercased text array. Lowercasing is
-- applied to individual code units, so the indexes into the lowercased text can
-- be used to index into the original text. It is still the responsibility of
-- the caller to lowercase the needles. Needles that contain uppercase code
-- points will not match.
--
-- NOTE: To get full advantage of inlining this function, you probably want to
-- compile the compiling module with -fllvm and the same optimization flags as
-- this module.
{-# INLINE runLower #-}
runLower :: AcMachine v -> Text -> [Match v]
runLower machine haystack = runText machine (lowerUtf16 haystack)

-- | Return a Text as a list of UTF-16 code units.
unpackUtf16 :: Text -> [CodeUnit]
unpackUtf16 (Text u16data offset length) =
  let
    go _ 0 = []
    go i n = TextArray.unsafeIndex u16data i : go (i + 1) (n - 1)
  in
    go offset length

-- | Return whether the code unit at the given index starts a surrogate pair.
-- Such a code unit must be followed by a high surrogate in valid UTF-16.
-- Returns false on out of bounds indices.
{-# INLINE isLowSurrogate #-}
isLowSurrogate :: Int -> Text -> Bool
isLowSurrogate !i (Text !u16data !offset !len) =
  let
    w = TextArray.unsafeIndex u16data (offset + i)
  in
    i >= 0 && i < len && w >= 0xd800 && w <= 0xdbff

-- | Return whether the code unit at the given index ends a surrogate pair.
-- Such a code unit must be preceded by a low surrogate in valid UTF-16.
-- Returns false on out of bounds indices.
{-# INLINE isHighSurrogate #-}
isHighSurrogate :: Int -> Text -> Bool
isHighSurrogate !i (Text !u16data !offset !len) =
  let
    w = TextArray.unsafeIndex u16data (offset + i)
  in
    i >= 0 && i < len && w >= 0xdc00 && w <= 0xdfff

-- | Extract a substring from a text, at a code unit offset and length.
-- This is similar to `Text.take length . Text.drop begin`, except that the
-- begin and length are in code *units*, not code points, so we can slice the
-- UTF-16 array, and we don't have to walk the entire text to take surrogate
-- pairs into account. It is the responsibility of the user to not slice
-- surrogate pairs, and to ensure that the length is within bounds, hence this
-- function is unsafe.
{-# INLINE unsafeSliceUtf16 #-}
unsafeSliceUtf16 :: CodeUnitIndex -> CodeUnitIndex -> Text -> Text
unsafeSliceUtf16 (CodeUnitIndex !begin) (CodeUnitIndex !length) !text
  = assert (begin + length <= TextUnsafe.lengthWord16 text)
  $ assert (not $ isHighSurrogate begin text)
  $ assert (not $ isLowSurrogate (begin + length - 1) text)
  $ TextUnsafe.takeWord16 length $ TextUnsafe.dropWord16 begin text

-- | The complement of `unsafeSliceUtf16`: removes the slice, and returns the
-- part before and after. See `unsafeSliceUtf16` for details.
{-# INLINE unsafeCutUtf16 #-}
unsafeCutUtf16 :: CodeUnitIndex -> CodeUnitIndex -> Text -> (Text, Text)
unsafeCutUtf16 (CodeUnitIndex !begin) (CodeUnitIndex !length) !text
  = assert (begin + length <= TextUnsafe.lengthWord16 text)
  $ assert (not $ isHighSurrogate begin text)
  $ assert (not $ isLowSurrogate (begin + length - 1) text)
    ( TextUnsafe.takeWord16 begin text
    , TextUnsafe.dropWord16 (begin + length) text
    )

-- | Return the length of the text, in number of code units.
{-# INLINE lengthUtf16 #-}
lengthUtf16 :: Text -> CodeUnitIndex
lengthUtf16 = CodeUnitIndex . TextUnsafe.lengthWord16

-- | Apply a function to each code unit of a text.
mapUtf16 :: (CodeUnit -> CodeUnit) -> Text -> Text
mapUtf16 f (Text u16data offset length) =
  let
    get !i = f $ TextArray.unsafeIndex u16data (offset + i)
    !(PVector.Vector !offset' !length' !(ByteArray !u16data')) =
      PVector.generate length get
  in
    Text (TextArray.Array u16data') offset' length'

-- | Lowercase each individual code unit of a text without changing their index.
-- This is not a proper case folding, but it does ensure that indices into the
-- lowercased string correspond to indices into the original string.
--
-- Differences from `Text.toLower` include code points in the BMP that lowercase
-- to multiple code points, and code points outside of the BMP.
--
-- For example, "İ" (U+0130), which `toLower` converts to "i" (U+0069, U+0307),
-- is converted into U+0069 only by `lowerUtf16`.
-- Also, "𑢢" (U+118A2), a code point from the Warang City writing system in the
-- Supplementary Multilingual Plane, introduced in 2014 to Unicode 7. It would
-- be lowercased to U+118C2 by `toLower`, but it is left untouched by
-- `lowerUtf16`.
lowerUtf16 :: Text -> Text
lowerUtf16 = mapUtf16 lowerCodeUnit

{-# INLINE lowerCodeUnit #-}
lowerCodeUnit :: CodeUnit -> CodeUnit
lowerCodeUnit cu =
  if cu >= 0xd800 && cu < 0xe000
     -- This code unit is part of a surrogate pair. Don't touch those, because
     -- we don't have all information required to decode the code point. Note
     -- that alphabets that need to be encoded as surrogate pairs are mostly
     -- archaic and obscure; all of the languages used by our customers have
     -- alphabets in the Basic Multilingual Plane, which does not need surrogate
     -- pairs. Note that the BMP is not just ascii or extended ascii. See also
     -- https://codepoints.net/basic_multilingual_plane.
    then cu
     -- The code unit is a code point on its own (not part of a surrogate pair),
     -- lowercase the code point. These code points, which are all in the BMP,
     -- have the important property that lowercasing them is again a code point
     -- in the BMP, so the output can be encoded in exactly one code unit, just
     -- like the input. This property was verified by exhaustive testing; see
     -- also the test in AhoCorasickSpec.hs.
    else fromIntegral $ Char.ord $ Char.toLower $ Char.chr $ fromIntegral cu
