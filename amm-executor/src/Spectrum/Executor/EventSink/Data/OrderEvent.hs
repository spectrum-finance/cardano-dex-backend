module Spectrum.Executor.EventSink.Data.OrderEvent
  ( PendingOrder(..)
  ) where

import Spectrum.Executor.Types
  ( Order )

newtype PendingOrder st = PendingOrder (st Order)
