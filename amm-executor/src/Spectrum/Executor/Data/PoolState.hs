module Spectrum.Executor.Data.PoolState
  ( PoolState(..)
  ) where

data PoolState
  = New
  | Consumed
  | Discarded
  deriving (Eq, Show)
