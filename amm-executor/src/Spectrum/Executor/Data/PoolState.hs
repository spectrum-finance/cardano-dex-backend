module Spectrum.Executor.Data.PoolState
  ( NewPool(..)
  , DiscardedPool(..)
  ) where

import Spectrum.Executor.Types
  ( PoolStateId, Pool )
import ErgoDex.Amm.Pool
  ( PoolId )

newtype NewPool st = NewPool (st Pool)

data DiscardedPool = DiscardedPool PoolId PoolStateId
