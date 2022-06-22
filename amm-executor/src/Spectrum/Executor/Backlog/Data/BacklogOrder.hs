module Spectrum.Executor.Backlog.Data.BacklogOrder
  ( BacklogOrder(..)
  , WeightedOrder(..)
  , mkWeightedOrder
  ) where

import Spectrum.Executor.Data.OrderState
  ( OrderState )
import Spectrum.Executor.Types
  ( Order, OrderId, OrderWeight, weightOrder, orderId )

data BacklogOrder = BacklogOrder
  { orderState   :: OrderState
  , backlogOrder :: Order
  }

data WeightedOrder = WeightedOrder OrderId OrderWeight
  deriving (Eq, Show)

mkWeightedOrder :: Order -> WeightedOrder
mkWeightedOrder ord = WeightedOrder (orderId ord) (weightOrder ord)

instance Ord WeightedOrder where
  compare (WeightedOrder _ xw) (WeightedOrder _ yw) = xw `compare` yw
