module Spectrum.Executor.PoolTracker.Service where

import ErgoDex.Amm.Pool (PoolId)
import Spectrum.Executor.Data.PoolState (Pool, Predicted)
import Spectrum.Executor.PoolTracker.Data.Traced (Traced)

data PoolResolver m = PoolResolver
  { resolvePool :: PoolId -> m (Maybe Pool)
  , putPool     :: Traced (Predicted Pool) -> m ()
  --, invalidatePool :: 
  }
