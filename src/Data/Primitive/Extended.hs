-- | "Data.Primitive" extended with extra definitions.
--
-- Based on the ".Extended Modules" pattern:
-- https://jaspervdj.be/posts/2015-01-20-haskell-design-patterns-extended-modules.html
module Data.Primitive.Extended
  ( module Data.Primitive
  , replicateMutablePrimArray
  )
  where

import Data.Primitive
import Control.Monad.Primitive (PrimMonad(..))

-- | Like 'replicatePrimArray', but does not freeze the array afterwards and
-- stays within a monadic context, so it can easily be mutated further.
{-# INLINE replicateMutablePrimArray #-}
replicateMutablePrimArray :: (Prim a, PrimMonad m) => Int -> a -> m (MutablePrimArray (PrimState m) a)
replicateMutablePrimArray len value = do
    arr <- newPrimArray len
    setPrimArray arr 0 len value
    pure arr
