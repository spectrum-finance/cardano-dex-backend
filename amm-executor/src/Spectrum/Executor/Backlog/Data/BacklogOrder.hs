module Spectrum.Executor.Backlog.Data.BacklogOrder
  ( BacklogOrder(..)
  ) where

import Spectrum.Executor.Data.OrderState (OrderState)
import Spectrum.Executor.Types (Order)

data BacklogOrder = BacklogOrder
  { orderState   :: OrderState
  , backlogOrder :: Order
  }
