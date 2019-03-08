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
  , Next (..)
  , lengthUtf16
  , lowerUtf16
  , unpackUtf16
  , unsafeCutUtf16
  , unsafeSliceUtf16
  )
  where

import Prelude hiding (length)

import Control.DeepSeq (NFData)
import Control.Exception (assert)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Foldable (foldl')
import Data.Hashable (Hashable)
import Data.IntMap.Strict (IntMap)
import Data.Text.Internal (Text (..))
import Data.Word (Word16, Word64)
import GHC.Generics (Generic)
import Data.Primitive.ByteArray (ByteArray (..))

import qualified Data.Char as Char
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Text.Array as TextArray
import qualified Data.Text.Unsafe as TextUnsafe
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Primitive as PVector

data CaseSensitivity
  = CaseSensitive
  | IgnoreCase
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Hashable, NFData)

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
  deriving newtype (Hashable, Ord, Num, Bounded, NFData)

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
  , machineTransitions :: !(UVector.Vector Transition)
  -- ^ A packed vector of transitions. For every state, there is a slice of this
  -- vector that starts at the offset given by `machineOffsets`, and ends at the
  -- first wildcard transition.
  , machineOffsets :: !(UVector.Vector Int)
  -- ^ For every state, the index into `machineTransitions` where the transition
  -- list for that state starts.
  , machineRootAsciiTransitions :: !(UVector.Vector Transition)
  -- ^ A lookup table for transitions from the root state, an optimization to
  -- avoid having to walk all transitions, at the cost of using a bit of
  -- additional memory.
  } deriving (Generic)

instance NFData v => NFData (AcMachine v)

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

-- | Pack transitions for each state into one contiguous array. In order to find
-- the transitions for a specific state, we also produce a vector of start
-- indices. All transition lists are terminated by a wildcard transition, so
-- there is no need to record the length.
packTransitions :: [[Transition]] -> (UVector.Vector Transition, UVector.Vector Int)
packTransitions transitions =
  let
    packed = UVector.fromList $ concat transitions
    offsets = UVector.fromList $ scanl (+) 0 $ fmap List.length transitions
  in
    (packed, offsets)

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
    (transitions, offsets) = packTransitions transitionsList
    rootTransitions = buildAsciiTransitionLookupTable $ transitionMap IntMap.! 0
    values = Vector.generate numStates (valueMap IntMap.!)
  in
    AcMachine values transitions offsets rootTransitions

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
    stateInitial = 0
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

-- Define aliases for array indexing so we can turn bounds checks on and off
-- in one place. We ran this code with `Vector.!` (bounds-checked indexing) in
-- production for two months without failing the bounds check, so we have turned
-- the check off for performance now.
at :: forall a. Vector.Vector a -> Int -> a
at = Vector.unsafeIndex

uAt :: forall a. UVector.Unbox a => UVector.Vector a -> Int -> a
uAt = UVector.unsafeIndex

-- | Result of handling a match: stepping the automaton can exit early by
-- returning a `Done`, or it can continue with a new accumulator with `Step`.
data Next a
  = Done !a
  | Step !a

-- | Run the automaton, possibly lowercasing the input text on the fly if case
-- insensitivity is desired. See also `lowerCodeUnit` and `runLower`.
-- WARNING: Run benchmarks when modifying this function; its performance is
-- fragile. It took many days to discover the current formulation which compiles
-- to fast code; removing the wrong bang pattern could cause a 10% performance
-- regression.
{-# INLINE runWithCase #-}
runWithCase
  :: forall a v
  .  CaseSensitivity
  -> a
  -> (a -> Match v -> Next a)
  -> AcMachine v
  -> Text
  -> a
runWithCase caseSensitivity seed f machine text =
  let
    Text u16data !initialOffset !initialRemaining = text
    !values = machineValues machine
    !transitions = machineTransitions machine
    !offsets = machineOffsets machine
    !rootAsciiTransitions = machineRootAsciiTransitions machine
    !stateInitial = 0

    -- NOTE: All of the arguments are strict here, because we want to compile
    -- them down to unpacked variables on the stack, or even registers.
    -- The INLINE / NOINLINE annotations here were added to fix a regression we
    -- observed when going from GHC 8.2 to GHC 8.6, and this particular
    -- combination of INLINE and NOINLINE is the fastest one. Removing increases
    -- the benchmark running time by about 9%.

    {-# NOINLINE consumeInput #-}
    consumeInput :: Int -> Int -> a -> State -> a
    consumeInput !offset !remaining !acc !state =
      let
        inputCodeUnit = fromIntegral $ TextArray.unsafeIndex u16data offset
        -- NOTE: Although doing this match here entangles the automaton a bit
        -- with case sensitivity, doing so is faster than passing in a function
        -- that transforms each code unit.
        casedCodeUnit = case caseSensitivity of
          IgnoreCase -> lowerCodeUnit inputCodeUnit
          CaseSensitive -> inputCodeUnit
      in
        case remaining of
          0 -> acc
          _ -> followEdge (offset + 1) (remaining - 1) acc state casedCodeUnit

    {-# INLINE followEdge #-}
    followEdge :: Int -> Int -> a -> State -> CodeUnit -> a
    followEdge !offset !remaining !acc !state !input =
      let
        !tssOffset = offsets `uAt` state
      in
        -- When we follow an edge, we look in the transition table and do a
        -- linear scan over all transitions until we find the right one, or
        -- until we hit the wildcard transition at the end. For 0 or 1 or 2
        -- transitions that is fine, but the initial state often has more
        -- transitions, so we have a dedicated lookup table for it, that takes
        -- up a bit more space, but provides O(1) lookup of the next state. We
        -- only do this for the first 128 code units (all of ascii).
        if state == stateInitial && input < asciiCount
          then lookupRootAsciiTransition offset remaining acc input
          else lookupTransition offset remaining acc state input tssOffset

    {-# NOINLINE collectMatches #-}
    collectMatches :: Int -> Int -> a -> State -> a
    collectMatches !offset !remaining !acc !state =
      let
        matchedValues = values `at` state
        -- Fold over the matched values. If at any point the user-supplied fold
        -- function returns `Done`, then we early out. Otherwise continue.
        handleMatch !acc' vs = case vs of
          []     -> consumeInput offset remaining acc' state
          v:more -> case f acc' (Match (CodeUnitIndex $ offset - initialOffset) v) of
            Step newAcc -> handleMatch newAcc more
            Done finalAcc -> finalAcc
      in
        handleMatch acc matchedValues

    -- NOTE: there is no `state` argument here, because this case applies only
    -- to the root state `stateInitial`.
    {-# INLINE lookupRootAsciiTransition #-}
    lookupRootAsciiTransition :: Int -> Int -> a -> CodeUnit -> a
    lookupRootAsciiTransition !offset !remaining !acc !input =
      case rootAsciiTransitions `uAt` fromIntegral input of
        t | transitionIsWildcard t -> consumeInput offset remaining acc stateInitial
          | otherwise -> collectMatches offset remaining acc (transitionState t)

    {-# INLINE lookupTransition #-}
    lookupTransition :: Int -> Int -> a -> State -> CodeUnit -> Int -> a
    lookupTransition !offset !remaining !acc !state !input !i =
      case transitions `uAt` i of
        -- There is no transition for the given input. Follow the fallback edge,
        -- and try again from that state, etc. If we are in the base state
        -- already, then nothing matched, so move on to the next input.
        t | transitionIsWildcard t ->
              if state == stateInitial
                then consumeInput offset remaining acc state
                else followEdge offset remaining acc (transitionState t) input

        -- We found the transition, switch to that new state, collecting matches.
        -- NOTE: This comes after wildcard checking, because the code unit of
        -- the wildcard transition is 0, which is a valid input.
        t | transitionCodeUnit t == input ->
              collectMatches offset remaining acc (transitionState t)

        -- The transition we inspected is not for the current input, and it is not
        -- a wildcard either; look at the next transition then.
        _ -> lookupTransition offset remaining acc state input (i + 1)
  in
    consumeInput initialOffset initialRemaining seed stateInitial

-- NOTE: To get full advantage of inlining this function, you probably want to
-- compile the compiling module with -fllvm and the same optimization flags as
-- this module.
{-# INLINE runText #-}
runText :: forall a v. a -> (a -> Match v -> Next a) -> AcMachine v -> Text -> a
runText = runWithCase CaseSensitive

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
runLower :: forall a v. a -> (a -> Match v -> Next a) -> AcMachine v -> Text -> a
runLower = runWithCase IgnoreCase

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
