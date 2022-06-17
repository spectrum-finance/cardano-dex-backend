module Spectrum.Executor.Data.OrderState
  ( OrderState(..)
  , OrderInState(..)
  ) where

import Spectrum.Executor.Types (Order)

data OrderState
  = Pending
  | Suspended
  | InProgress
  deriving (Eq, Show)

data OrderInState st where
  PendingOrder :: Order -> OrderInState 'Pending
  SuspendedOrder :: Order -> OrderInState 'Suspended
  InProgressOrder :: Order -> OrderInState 'InProgress
