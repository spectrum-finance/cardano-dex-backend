module Spectrum.Executor.Backlog.Data.BacklogOrder
  ( BacklogOrder(..)
  , WeightedOrder(..)
  , mkWeightedOrder
  ) where

import RIO 
  ( Generic )
import RIO.Time 
  ( UTCTime )
import Data.Aeson.Types 
  ( ToJSON, FromJSON )

import Spectrum.Executor.Types
  ( Order, OrderId, OrderWeight, weightOrder, orderId )

data BacklogOrder = BacklogOrder
  { orderTimestamp :: UTCTime
  , backlogOrder   :: Order
  } deriving (Generic, ToJSON, FromJSON, Show)

data WeightedOrder = WeightedOrder OrderId OrderWeight UTCTime
  deriving (Eq, Show)

mkWeightedOrder :: Order -> UTCTime -> WeightedOrder
mkWeightedOrder ord = WeightedOrder (orderId ord) (weightOrder ord)

instance Ord WeightedOrder where
  compare (WeightedOrder _ xw _) (WeightedOrder _ yw _) = xw `compare` yw
