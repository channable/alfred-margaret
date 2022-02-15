-- Alfred-Margaret: Fast Aho-Corasick string searching
-- Copyright 2022 Channable
--
-- Licensed under the 3-clause BSD license, see the LICENSE file in the
-- repository root.

{-# LANGUAGE ScopedTypeVariables #-}

-- | UTF-8 version of 'Data.Text.AhoCorasick.Automaton'
module Data.Text.Utf8.AhoCorasick.Automaton where

import           Data.Primitive.ByteArray (ByteArray)
import           Data.Text.Utf8           (CodeUnit, CodeUnitIndex, unpackUtf8)
import qualified Data.Vector              as Vector
import qualified Data.Vector.Unboxed      as UVector
import           Data.Word                (Word64)

-- TODO: Transition to text-2.0
type HayStack = ByteArray

-- TODO: Since 2^16 states are (probably?) enough, we could turn this into a Word32 instead,
-- where the msb are the state number and the lsb are the input code unit.
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

build :: [([CodeUnit], v)] -> AcMachine v
build = error "not implemented"

data Next a = Done !a | Step !a

{-# INLINE runWithCase #-}
runWithCase
  :: forall a v
  .  ()
  -> a
  -> (a -> Match v -> Next a)
  -> AcMachine v
  -> HayStack
  -> a
runWithCase () seed f machine text =
    undefined

{-# INLINE runText #-}
runText :: forall a v. a -> (a -> Match v -> Next a) -> AcMachine v -> HayStack -> a
runText = runWithCase ()
