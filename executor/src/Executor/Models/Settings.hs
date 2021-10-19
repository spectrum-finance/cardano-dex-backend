module Executor.Models.Settings 
    ( KafkaConsumerSettings(..)
    , PoolsResolverClientSettings(..)
    , HasKafkaConsumerSettings(..)
    , HasSettings(..)
    , AppSettings(..)
    , PaymentSettings(..)
    ) where

import RIO
import Dhall

data AppSettings = AppSettings
  { getKafkaSettings :: KafkaConsumerSettings
  , getHttpSettings  :: PoolsResolverClientSettings
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

data PoolsResolverClientSettings = PoolsResolverClientSettings
  { getHost :: String
  , getPort :: Natural
  } deriving (Generic, Show)

instance FromDhall PoolsResolverClientSettings

data PaymentSettings = PaymentSettings
  { pubKeyHash :: Text
  , feeAddr    :: Text
  } deriving (Generic, Show)

instance FromDhall PaymentSettings

class HasKafkaConsumerSettings env where
  kafkaSettingsL :: Lens' env KafkaConsumerSettings
instance HasKafkaConsumerSettings KafkaConsumerSettings where
  kafkaSettingsL = id
instance HasKafkaConsumerSettings AppSettings where
  kafkaSettingsL = lens getKafkaSettings (\x y -> x { getKafkaSettings = y })

class HasSettings env where
  httpSettingsL :: Lens' env PoolsResolverClientSettings
instance HasSettings PoolsResolverClientSettings where
  httpSettingsL = id
instance HasSettings AppSettings where
  httpSettingsL = lens getHttpSettings (\x y -> x { getHttpSettings = y })