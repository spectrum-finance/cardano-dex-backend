module Spectrum.Executor.Data.PoolState
  ( PoolState(..)
  ) where

import Data.Aeson
  ( ToJSON, FromJSON )
import GHC.Generics
  ( Generic )

data PoolState
  = New
  | Consumed
  | Discarded
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
