module Executor.Models.Settings 
    ( PoolsResolverConfig(..)
    , PaymentConfig(..)
    , AppConfig(..)
    ) where

import RIO
import Dhall

import Streaming.Config
import Streaming.Types

data AppConfig = AppConfig
  { kafkaConfig         :: KafkaConsumerConfig
  , topicId             :: TopicId
  , poolsResolverConfig :: PoolsResolverConfig
  , paymentConfig       :: PaymentConfig
  } deriving (Generic)

instance FromDhall AppConfig

data PoolsResolverConfig = PoolsResolverConfig
  { getHost :: String
  , getPort :: Natural
  } deriving (Generic, Show)

instance FromDhall PoolsResolverConfig

data PaymentConfig = PaymentConfig
  { pubKeyHash :: Text
  , feeAddr    :: Text
  } deriving (Generic, Show)

instance FromDhall PaymentConfig