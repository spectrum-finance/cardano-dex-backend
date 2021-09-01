module Resolver.Models.AppSettings
  ( KafkaConsumerSettings(..)
  , HasKafkaConsumerSettings(..)
  , HttpServerSettings(..)
  , HasHttpServerSettings(..)
  , AppSettings(..)
  , HasAppSettings(..)
  , RedisSettings(..)
  ) where

import RIO
import Kafka.Consumer
import Dhall

data AppSettings = AppSettings
    { getKafkaSettings :: KafkaConsumerSettings
    , getHttpSettings :: HttpServerSettings
    , redisSettings :: RedisSettings
    } deriving (Generic)

instance FromDhall AppSettings

data KafkaConsumerSettings = KafkaConsumerSettings
  { getBrokerList :: [Text]
  , getGroupId :: Text
  , getTopicsList :: [Text]
  , getPollRate :: Natural
  , getBatchSize :: Natural
  } deriving (Generic)

instance FromDhall KafkaConsumerSettings

data HttpServerSettings = HttpServerSettings
  { getPort :: Natural
  , getHost :: String
  } deriving (Generic)

instance FromDhall HttpServerSettings

data RedisSettings = RedisSettings
  { getRedisHost :: String 
  } deriving (Generic)

instance FromDhall RedisSettings

class HasKafkaConsumerSettings env where
  kafkaSettingsL :: Lens' env KafkaConsumerSettings
instance HasKafkaConsumerSettings KafkaConsumerSettings where
  kafkaSettingsL = id
instance HasKafkaConsumerSettings AppSettings where
  kafkaSettingsL = lens getKafkaSettings (\x y -> x { getKafkaSettings = y })

class HasHttpServerSettings env where
  httpSettingsL :: Lens' env HttpServerSettings
instance HasHttpServerSettings HttpServerSettings where
  httpSettingsL = id
instance HasHttpServerSettings AppSettings where
  httpSettingsL = lens getHttpSettings (\x y -> x { getHttpSettings = y })

class HasAppSettings env where
  appSettingsL :: Lens' env AppSettings
instance HasAppSettings AppSettings where
  appSettingsL = id