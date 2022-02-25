{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Text.CaseSensitivity where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data CaseSensitivity
  = CaseSensitive
  | IgnoreCase
  deriving stock (Eq, Generic, Show)
#if defined(HAS_AESON)
  deriving anyclass (Hashable, NFData, FromJSON, ToJSON)
#else
  deriving anyclass (Hashable, NFData)
#endif
