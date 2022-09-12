module Spectrum.Executor.Data.PoolState
  ( NewPool(..)
  , DiscardedPool(..)
  ) where

import Spectrum.Executor.Types
  ( PoolStateId, Pool )
import ErgoDex.Amm.Pool
  ( PoolId )

newtype NewPool st = NewPool (st Pool)

instance Show (st Pool) => Show (NewPool st) where
  show (NewPool a) = show a 

data DiscardedPool = DiscardedPool PoolId PoolStateId
