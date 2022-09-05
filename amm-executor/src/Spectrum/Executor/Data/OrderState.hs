module Spectrum.Executor.Data.OrderState
  ( OrderState(..)
  , OrderInState(..)
  ) where

import Spectrum.Executor.Types (Order, OrderId)
import RIO.Time (UTCTime)

data OrderState
  = Pending
  | Suspended
  | InProgress
  | Eliminated
  deriving (Eq, Show)

data OrderInState st where
  PendingOrder    :: Order -> UTCTime -> OrderInState 'Pending
  SuspendedOrder  :: Order -> UTCTime -> OrderInState 'Suspended
  InProgressOrder :: Order -> UTCTime -> OrderInState 'InProgress
  EliminatedOrder :: OrderId -> OrderInState 'Eliminated
deriving instance Show (OrderInState st)