module Resolver.Models.AppSettings
  ( KafkaConsumerSettings(..)
  , HasKafkaConsumerSettings(..)
  , HttpServerSettings(..)
  , HasHttpServerSettings(..)
  , AppSettings(..)
  , HasAppSettings(..)
  ) where

import RIO
import Kafka.Consumer

data AppSettings = AppSettings
    { getKafkaSettings :: KafkaConsumerSettings
    , getHttpSettings :: HttpServerSettings
    }

data KafkaConsumerSettings = KafkaConsumerSettings
  { getBrokerList :: [BrokerAddress]
  , getGroupId :: ConsumerGroupId
  , getTopicsList :: [TopicName]
  , getPollRate :: Int
  , getBatchSize :: Int
  }

data HttpServerSettings = HttpServerSettings
  { getPort :: Int
  , getHost :: String
  }

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