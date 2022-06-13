module Spectrum.Executor.EventSink.Data.PoolEvent
  ( PoolEvent(..)
  , type NewPoolEvent
  ) where

import qualified Spectrum.Executor.Data.PoolState as State

data PoolEvent st p where
  NewPool :: p ->  PoolEvent 'State.New p

type NewPoolEvent p = PoolEvent 'State.New p
