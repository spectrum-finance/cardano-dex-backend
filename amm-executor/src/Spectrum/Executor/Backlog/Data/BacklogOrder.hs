module Spectrum.Executor.Backlog.Data.BacklogOrder
  ( BacklogOrder(..)
  , WeightedOrderWithTimestamp(..)
  , mkWeightedOrderWithTimestamp
  ) where

import Spectrum.Executor.Types
  ( Order, OrderId, OrderWeight, weightOrder, orderId )
import RIO.Time (UTCTime)
import Data.Aeson.Types (ToJSON, FromJSON)
import RIO (Generic)

data BacklogOrder = BacklogOrder
  { orderTimestamp :: UTCTime
  , backlogOrder   :: Order
  } deriving (Generic, ToJSON, FromJSON, Show)

data WeightedOrderWithTimestamp = WeightedOrderWithTimestamp OrderId OrderWeight UTCTime
  deriving (Eq, Show)

mkWeightedOrderWithTimestamp :: Order -> UTCTime -> WeightedOrderWithTimestamp
mkWeightedOrderWithTimestamp ord = WeightedOrderWithTimestamp (orderId ord) (weightOrder ord)

instance Ord WeightedOrderWithTimestamp where
  compare (WeightedOrderWithTimestamp _ xw _) (WeightedOrderWithTimestamp _ yw _) = xw `compare` yw
