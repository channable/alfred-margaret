-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2022 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE BangPatterns #-}
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
--
-- This module is a rewrite of the previous version which used an older version of
-- the 'text' package which in turn used UTF-16 internally.
module Data.Text.Utf8.AhoCorasick.Automaton
    ( AcMachine (..)
    , CaseSensitivity (..)
    , CodeUnitIndex (..)
    , Match (..)
    , Next (..)
    , build
    , debugBuildDot
    , runLower
    , runText
    , runWithCase
    ) where

import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.Char (chr)
import Data.Foldable (foldl')
import Data.Functor ((<&>))
import Data.IntMap.Strict (IntMap)
import Data.Word (Word32, Word64)

import qualified Data.Char as Char
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Vector as Vector

import Data.Text.CaseSensitivity (CaseSensitivity (..))
import Data.Text.Utf8 (CodeUnit, CodeUnitIndex (CodeUnitIndex), Text (..), indexTextArray)
import Data.TypedByteArray (Prim, TypedByteArray)

import qualified Data.Text.Utf8 as Utf8
import qualified Data.TypedByteArray as TBA

-- TYPES
-- | A numbered state in the Aho-Corasick automaton.
type State = Int

-- | A transition is a pair of (code point, next state). The code point is 21 bits,
-- and the state index is 32 bits. The code point is stored in
-- the least significant 32 bits, with the special value 2^21 indicating a
-- wildcard; the "failure" transition. Bits 22 through 31 (starting from zero,
-- both bounds inclusive) are always 0.
--
--
-- >  Bit 63 (most significant)                 Bit 0 (least significant)
-- >  |                                                                 |
-- >  v                                                                 v
-- > |<--       goto state         -->|<-- 0s -->| |<--     input     -->|
-- > |SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS|0000000000|W|IIIIIIIIIIIIIIIIIIIII|
-- >                                              |
-- >                                        Wildcard bit (bit 21)
--
-- If you change this representation, make sure to update 'transitionCodeUnit',
-- 'wildcard', 'transitionState', 'transitionIsWildcard', 'newTransition' and
-- 'newWildcardTransition' as well. Those functions form the interface used to
-- construct and read transitions.
type Transition = Word64

type Offset = Word32

data Match v = Match
  { matchPos   :: {-# UNPACK #-} !CodeUnitIndex
  -- ^ The code unit index past the last code unit of the match. Note that this
  -- is not a code *point* (Haskell `Char`) index; a code point might be encoded
  -- as up to four code units.
  , matchValue :: v
  -- ^ The payload associated with the matched needle.
  }

-- | An Aho-Corasick automaton.
data AcMachine v = AcMachine
  { machineValues               :: !(Vector.Vector [v])
  -- ^ For every state, the values associated with its needles. If the state is
  -- not a match state, the list is empty.
  , machineTransitions          :: !(TypedByteArray Transition)
  -- ^ A packed vector of transitions. For every state, there is a slice of this
  -- vector that starts at the offset given by `machineOffsets`, and ends at the
  -- first wildcard transition.
  , machineOffsets              :: !(TypedByteArray Offset)
  -- ^ For every state, the index into `machineTransitions` where the transition
  -- list for that state starts.
  , machineRootAsciiTransitions :: !(TypedByteArray Transition)
  -- ^ A lookup table for transitions from the root state, an optimization to
  -- avoid having to walk all transitions, at the cost of using a bit of
  -- additional memory.
  }

type CodePoint = Int

-- AUTOMATON CONSTRUCTION

-- | The wildcard value is 2^21, one more than the maximal 21-bit code point.
wildcard :: Integral a => a
wildcard = 0x200000

-- | Extract the code unit from a transition. The special wildcard transition
-- will return 0.
transitionCodeUnit :: Transition -> CodePoint
transitionCodeUnit t = fromIntegral (t .&. 0x1fffff)

-- | Extract the goto state from a transition.
transitionState :: Transition -> State
transitionState t = fromIntegral (t `shiftR` 32)

-- | Test if the transition is not for a specific code unit, but the wildcard
-- transition to take if nothing else matches.
transitionIsWildcard :: Transition -> Bool
transitionIsWildcard t = (t .&. wildcard) == wildcard

newTransition :: CodePoint -> State -> Transition
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
packTransitions :: [[Transition]] -> (TypedByteArray Transition, TypedByteArray Offset)
packTransitions transitions =
  let
    packed = TBA.fromList $ concat transitions
    offsets = TBA.fromList $ map fromIntegral $ scanl (+) 0 $ fmap List.length transitions
  in
    (packed, offsets)

-- | Construct an Aho-Corasick automaton for the given needles.
-- The automaton uses Unicode code points to match the input.
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
      "  " ++ show state ++ " -> " ++ show nextState ++ " [" ++ extra ++ "];"

    dotFallbackEdge :: [String] -> State -> State -> [String]
    dotFallbackEdge edges state nextState =
      dotEdge "style = dashed" state nextState : edges

    dotTransitionEdge :: State -> [String] -> Int -> State -> [String]
    dotTransitionEdge state edges input nextState =
      dotEdge ("label = \"" ++ showInput input ++ "\"") state nextState : edges

    showInput input = [chr input]

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
    unlines $ ["digraph {", "  rankdir = \"LR\";"] ++ reverse dot2 ++ ["}"]

-- Different int maps that are used during constuction of the automaton. The
-- transition map represents the trie of states, the fallback map contains the
-- fallback (or "failure" or "suffix") edge for every state.
type TransitionMap = IntMap (IntMap State)
type FallbackMap = IntMap State
type ValuesMap v = IntMap [v]

-- | Build the trie of the Aho-Corasick state machine for all input needles.
buildTransitionMap :: forall v. [([CodeUnit], v)] -> (Int, TransitionMap, ValuesMap v)
buildTransitionMap needles = buildTransitionMap' [(decodeUtf8 cus, val) | (cus, val) <- needles]

-- | Decode a list of UTF-8 code units into a list of code points.
decodeUtf8 :: [CodeUnit] -> [CodePoint]
decodeUtf8 [] = []
decodeUtf8 (cu0 : cus) | cu0 < 0xc0 = fromIntegral cu0 : decodeUtf8 cus
decodeUtf8 (cu0 : cu1 : cus) | cu0 < 0xe0 = Utf8.decode2 cu0 cu1 : decodeUtf8 cus
decodeUtf8 (cu0 : cu1 : cu2 : cus) | cu0 < 0xf0 = Utf8.decode3 cu0 cu1 cu2 : decodeUtf8 cus
decodeUtf8 (cu0 : cu1 : cu2 : cu3 : cus) = Utf8.decode4 cu0 cu1 cu2 cu3 : decodeUtf8 cus
decodeUtf8 cus = error $ "Invalid UTF-8 input sequence at " ++ show (take 4 cus)

buildTransitionMap' :: forall v. [([CodePoint], v)] -> (Int, TransitionMap, ValuesMap v)
buildTransitionMap' =
  let
    -- | Inserts a single needle into the given transition and values map.
    -- Int is used to keep track of the current number of states.
    go :: State
      -> (Int, TransitionMap, ValuesMap v)
      -> ([CodePoint], v)
      -> (Int, TransitionMap, ValuesMap v)

    -- End of the current needle, insert the associated payload value.
    -- If a needle occurs multiple times, then at this point we will merge
    -- their payload values, so the needle is reported twice, possibly with
    -- different payload values.
    go !state (!numStates, transitions, values) ([], v) =
      (numStates, transitions, IntMap.insertWith (++) state [v] values)

    -- Follow the edge for the given input from the current state, creating it
    -- if it does not exist.
    go !state (!numStates, transitions, values) (input : needleTail, vs) =
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
                $ IntMap.insert nextState IntMap.empty transitions
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

-- | Build a lookup table for the first 128 code points, that can be used for
-- O(1) lookup of a transition, rather than doing a linear scan over all
-- transitions. The fallback goes back to the initial state, state 0.
buildAsciiTransitionLookupTable :: IntMap State -> TypedByteArray Transition
buildAsciiTransitionLookupTable transitions = TBA.fromList $ [0..asciiCount - 1] <&> \i ->
  case IntMap.lookup i transitions of
    Just state -> newTransition (fromIntegral i) state
    Nothing    -> newWildcardTransition 0

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
{-# INLINE at #-}
at :: forall a. Vector.Vector a -> Int -> a
at = Vector.unsafeIndex

{-# INLINE uAt #-}
uAt :: Prim a => TypedByteArray a -> Int -> a
uAt = TBA.unsafeIndex

-- RUNNING THE MACHINE

-- | Result of handling a match: stepping the automaton can exit early by
-- returning a `Done`, or it can continue with a new accumulator with `Step`.
data Next a = Done !a | Step !a

-- | Run the automaton, possibly lowercasing the input text on the fly if case
-- insensitivity is desired. See also `runLower`.
--
-- The code of this function itself is organized as a state machine as well.
-- Each state in the diagram below corresponds to a function defined in
-- `runWithCase`. These functions are written in a way such that GHC identifies them
-- as [join points](https://www.microsoft.com/en-us/research/publication/compiling-without-continuations/).
-- This means that they can be compiled to jumps instead of function calls, which helps performance a lot.
--
-- @
--   ┌─────────────────────────────┐
--   │                             │
-- ┌─▼──────────┐   ┌──────────────┴─┐   ┌──────────────┐
-- │consumeInput├───►lookupTransition├───►collectMatches│
-- └─▲──────────┘   └─▲────────────┬─┘   └────────────┬─┘
--   │                │            │                  │
--   │                └────────────┘                  │
--   │                                                │
--   └────────────────────────────────────────────────┘
-- @
--
-- * @consumeInput@ decodes a code point of up to four code units and possibly lowercases it.
--   It passes this code point to @followCodePoint@, which in turn calls @lookupTransition@.
-- * @lookupTransition@ checks whether the given code point matches any transitions at the given state.
--   If so, it follows the transition and calls @collectMatches@. Otherwise, it follows the fallback transition
--   and calls @followCodePoint@ or @consumeInput@.
-- * @collectMatches@ checks whether the current state is accepting and updates the accumulator accordingly.
--   Afterwards it loops back to @consumeInput@.
--
-- NOTE: @followCodePoint@ is actually inlined into @consumeInput@ by GHC.
-- It is included in the diagram for illustrative reasons only.
--
-- All of these functions have the arguments @offset@, @remaining@, @state@ and @acc@ which encode the current input
-- position and the accumulator, which contains the matches. If you change any of the functions above,
-- make sure to check the Core dumps afterwards that @offset@, @remaining@ and @state@ were turned
-- into unboxed @Int#@ by GHC. If any of them aren't, the program will constantly allocate and deallocate heap space for them.
-- You can nudge GHC in the right direction by using bang patterns on these arguments.
--
-- WARNING: Run benchmarks when modifying this function; its performance is
-- fragile. It took many days to discover the current formulation which compiles
-- to fast code; removing the wrong bang pattern could cause a 10% performance
-- regression.
{-# INLINE runWithCase #-}
runWithCase :: forall a v. CaseSensitivity -> a -> (a -> Match v -> Next a) -> AcMachine v -> Text -> a
runWithCase !caseSensitivity !seed !f !machine !text =
  consumeInput initialOffset initialRemaining seed initialState
  where
    initialState = 0

    Text !u8data !initialOffset !initialRemaining = text
    AcMachine !values !transitions !offsets !rootAsciiTransitions = machine

    -- NOTE: All of the arguments are strict here, because we want to compile
    -- them down to unpacked variables on the stack, or even registers.

    -- When we follow an edge, we look in the transition table and do a
    -- linear scan over all transitions until we find the right one, or
    -- until we hit the wildcard transition at the end. For 0 or 1 or 2
    -- transitions that is fine, but the initial state often has more
    -- transitions, so we have a dedicated lookup table for it, that takes
    -- up a bit more space, but provides O(1) lookup of the next state. We
    -- only do this for the first 128 code units (all of ascii).

    -- | Consume a code unit sequence that constitutes a full code point.
    -- If the code unit at @offset@ is ASCII, we can lower it using 'Utf8.toLowerAscii'.
    {-# NOINLINE consumeInput #-}
    consumeInput :: Int -> Int -> a -> State -> a
    consumeInput !_offset 0 !acc !_state = acc
    consumeInput !offset !remaining !acc !state =
      followCodePoint (offset + codeUnits) (remaining - codeUnits) acc possiblyLoweredCp state

      where
        !cu = indexTextArray u8data offset
        (!codeUnits, !cp)
          | cu < 0xc0 = (1, fromIntegral cu)
          | cu < 0xe0 = (2, Utf8.decode2 cu $ indexTextArray u8data $ offset + 1)
          | cu < 0xf0 = (3, Utf8.decode3 cu (indexTextArray u8data $ offset + 1) (indexTextArray u8data $ offset + 2))
          | otherwise = (4, Utf8.decode4 cu (indexTextArray u8data $ offset + 1) (indexTextArray u8data $ offset + 2) (indexTextArray u8data $ offset + 3))

        !possiblyLoweredCp = case caseSensitivity of
          CaseSensitive -> cp
          IgnoreCase
            | cp < asciiCount -> Utf8.toLowerAscii cp
            | otherwise -> Char.ord $ Char.toLower $ Char.chr cp

    {-# INLINE followCodePoint #-}
    followCodePoint :: Int -> Int -> a -> CodePoint -> State -> a
    followCodePoint !offset !remaining !acc !cp !state
      | state == initialState && cp < asciiCount = lookupRootAsciiTransition offset remaining acc cp
      | otherwise = lookupTransition offset remaining acc cp state $ offsets `uAt` state

    -- NOTE: This function can't be inlined since it is self-recursive.
    {-# NOINLINE lookupTransition #-}
    lookupTransition :: Int -> Int -> a -> CodePoint -> State -> Offset -> a
    lookupTransition !offset !remaining !acc !cp !state !i
      -- There is no transition for the given input. Follow the fallback edge,
      -- and try again from that state, etc. If we are in the base state
      -- already, then nothing matched, so move on to the next input.
      | transitionIsWildcard t =
        if state == initialState
          then consumeInput offset remaining acc state
          else followCodePoint offset remaining acc cp (transitionState t)
      -- We found the transition, switch to that new state, possibly matching the rest of cus.
      -- NOTE: This comes after wildcard checking, because the code unit of
      -- the wildcard transition is 0, which is a valid input.
      | transitionCodeUnit t == cp =
        collectMatches offset remaining acc (transitionState t)
      -- The transition we inspected is not for the current input, and it is not
      -- a wildcard either; look at the next transition then.
      | otherwise =
        lookupTransition offset remaining acc cp state $ i + 1

      where
        !t = transitions `uAt` fromIntegral i

    -- NOTE: there is no `state` argument here, because this case applies only
    -- to the root state `stateInitial`.
    {-# INLINE lookupRootAsciiTransition #-}
    lookupRootAsciiTransition !offset !remaining !acc !cp
      -- Given code unit does not match at root ==> Repeat at offset from initial state
      | transitionIsWildcard t = consumeInput offset remaining acc initialState
      -- Transition matched!
      | otherwise = collectMatches offset remaining acc $ transitionState t
      where !t = rootAsciiTransitions `uAt` fromIntegral cp

    {-# NOINLINE collectMatches #-}
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

-- NOTE: To get full advantage of inlining this function, you probably want to
-- compile the compiling module with -fllvm and the same optimization flags as
-- this module.
{-# INLINE runText #-}
runText :: forall a v. a -> (a -> Match v -> Next a) -> AcMachine v -> Text -> a
runText = runWithCase CaseSensitive

-- Finds all matches in the lowercased text. This function lowercases the input text
-- on the fly to avoid allocating a second lowercased text array.  It is still the
-- responsibility of  the caller to lowercase the needles. Needles that contain
-- uppercase code  points will not match.
--
-- NOTE: To get full advantage of inlining this function, you probably want to
-- compile the compiling module with -fllvm and the same optimization flags as
-- this module.
{-# INLINE runLower #-}
runLower :: forall a v. a -> (a -> Match v -> Next a) -> AcMachine v -> Text -> a
runLower = runWithCase IgnoreCase
