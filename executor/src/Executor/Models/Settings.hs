module Executor.Models.Settings 
    ( KafkaConsumerSettings(..)
    , PoolsResolverConfig(..)
    , AppSettings(..)
    , PaymentSettings(..)
    ) where

import RIO
import Dhall

data AppSettings = AppSettings
  { getKafkaSettings :: KafkaConsumerSettings
  , poolsResolverConfig  :: PoolsResolverConfig
  , paymentSettings  :: PaymentSettings
  } deriving (Generic)

instance FromDhall AppSettings

data KafkaConsumerSettings = KafkaConsumerSettings
  { getBrokerList :: [Text]
  , getGroupId    :: Text
  , getTopicsList :: [Text]
  , getPollRate   :: Natural
  , getBatchSize  :: Natural
  } deriving (Generic)

instance FromDhall KafkaConsumerSettings

data PoolsResolverConfig = PoolsResolverConfig
  { getHost :: String
  , getPort :: Natural
  } deriving (Generic, Show)

instance FromDhall PoolsResolverConfig

data PaymentSettings = PaymentSettings
  { pubKeyHash :: Text
  , feeAddr    :: Text
  } deriving (Generic, Show)

instance FromDhall PaymentSettings