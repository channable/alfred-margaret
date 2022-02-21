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
module Data.Text.Utf8.AhoCorasick.Automaton where

import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.Char (chr)
import Data.Foldable (foldl')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import Data.Text.Utf8 (CodeUnit, CodeUnitIndex (CodeUnitIndex), Text (..), indexTextArray)
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as UVector
import Data.Word (Word64)

-- TYPES
-- | A numbered state in the Aho-Corasick automaton.
type State = Int

-- | A transition is a pair of (code unit, next state). The code unit is 8 bits,
-- and the state index is 32 bits. We pack these together as a manually unlifted
-- tuple, because an unboxed Vector of tuples is a tuple of vectors, but we want
-- the elements of the tuple to be adjacent in memory. (The Word64 still needs
-- to be unpacked in the places where it is used.) The code unit is stored in
-- the least significant 32 bits, with the special value 2^8 indicating a
-- wildcard; the "failure" transition. Bit 9 through 31 (starting from zero,
-- both bounds inclusive) are always 0.
--
--  Bit 63 (most significant)                 Bit 0 (least significant)
--  |                                                                 |
--  v                                                                 v
-- |<--       goto state         -->|<--       zeros     -->| |<-input>|
-- |SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS|00000000000000000000000|W|IIIIIIII|
--                                                           |
--                                                   Wildcard bit (bit 16)
--
-- If you change this representation, make sure to update 'transitionCodeUnit',
-- 'wildcard', 'transitionState', 'transitionIsWildcard', 'newTransition' and
-- 'newWildcardTransition' as well. Those functions form the interface used to
-- construct and read transitions.
type Transition = Word64

data Match v = Match
  { matchPos   :: {-# UNPACK #-} !CodeUnitIndex
  -- ^ The code unit index past the last code unit of the match. Note that this
  -- is not a code *point* (Haskell `Char`) index; a code point might be encoded
  -- as two code units.
  , matchValue :: v
  -- ^ The payload associated with the matched needle.
  }

-- | An Aho-Corasick automaton.
data AcMachine v = AcMachine
  { machineValues               :: !(Vector.Vector [v])
  -- ^ For every state, the values associated with its needles. If the state is
  -- not a match state, the list is empty.
  , machineTransitions          :: !(UVector.Vector Transition)
  -- ^ A packed vector of transitions. For every state, there is a slice of this
  -- vector that starts at the offset given by `machineOffsets`, and ends at the
  -- first wildcard transition.
  , machineOffsets              :: !(UVector.Vector Int)
  -- ^ For every state, the index into `machineTransitions` where the transition
  -- list for that state starts.
  , machineRootAsciiTransitions :: !(UVector.Vector Transition)
  -- ^ A lookup table for transitions from the root state, an optimization to
  -- avoid having to walk all transitions, at the cost of using a bit of
  -- additional memory.
  }

-- AUTOMATON CONSTRUCTION

-- | The wildcard value is 2^8, one more than the maximal 8-bit code unit (255/0xff).
wildcard :: Integral a => a
wildcard = 0x100

-- | Extract the code unit from a transition. The special wildcard transition
-- will return 0.
transitionCodeUnit :: Transition -> CodeUnit
transitionCodeUnit t = fromIntegral (t .&. 0xff)

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
-- The automaton uses UTF-8 code units (bytes) to match the input.
-- This means that running it is a bit tricky if you want to ignore case,
-- since changing a code point's case may increase or decrease its number of code units.
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

    showInput input
      | input < 0x80 = [chr input]
      | otherwise     = "0x" ++ asHexByte input

    asHexByte input =
      [hexChars List.!! div input 16, hexChars List.!! mod input 16]
      where hexChars = ['0'..'9'] ++ ['a'..'f']

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
buildTransitionMap =
  let
    -- | Inserts a single needle into the given transition and values map.
    -- Int is used to keep track of the current number of states.
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

-- | Build a lookup table for the first 128 code units, that can be used for
-- O(1) lookup of a transition, rather than doing a linear scan over all
-- transitions. The fallback goes back to the initial state, state 0.
buildAsciiTransitionLookupTable :: IntMap State -> UVector.Vector Transition
buildAsciiTransitionLookupTable transitions = UVector.generate asciiCount $ \i ->
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
at :: forall a. Vector.Vector a -> Int -> a
at = Vector.unsafeIndex

uAt :: forall a. UVector.Unbox a => UVector.Vector a -> Int -> a
uAt = UVector.unsafeIndex

-- RUNNING THE MACHINE

-- | Result of handling a match: stepping the automaton can exit early by
-- returning a `Done`, or it can continue with a new accumulator with `Step`.
data Next a = Done !a | Step !a

-- | The unit parameter here is a placeholder for a future CaseSensitivity flag,
-- see old module.
{-# INLINE runWithCase #-}
runWithCase
  :: forall a v
  .  ()
  -> a
  -> (a -> Match v -> Next a)
  -> AcMachine v
  -> Text
  -> a
runWithCase () seed f machine (Text !u8data !initialOffset !initialRemaining) =
  let
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
        inputCodeUnit = fromIntegral $ indexTextArray u8data offset
        -- NOTE: Although doing this match here entangles the automaton a bit
        -- with case sensitivity, doing so is faster than passing in a function
        -- that transforms each code unit.
        -- casedCodeUnit = case caseSensitivity of
        --  IgnoreCase    -> lowerCodeUnit inputCodeUnit
        --  CaseSensitive -> inputCodeUnit
      in
        case remaining of
          0 -> acc
          _ -> followEdge (offset + 1) (remaining - 1) acc state inputCodeUnit

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
            Step newAcc   -> handleMatch newAcc more
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
runText = runWithCase ()
