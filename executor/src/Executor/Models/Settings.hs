module Executor.Models.Settings 
    ( KafkaConsumerSettings(..)
    , HttpSettings(..)
    , HasKafkaConsumerSettings(..)
    , HasHttpSettings(..)
    , AppSettings(..)
    ) where

import RIO
import Kafka.Consumer

data AppSettings = AppSettings
  { getKafkaSettings :: KafkaConsumerSettings
  , getHttpSettings :: HttpSettings
  }

data KafkaConsumerSettings = KafkaConsumerSettings
  { getBrokerList :: [BrokerAddress]
  , getGroupId :: ConsumerGroupId
  , getTopicsList :: [TopicName]
  , getPollRate :: Int
  , getBatchSize :: Int
  }

data HttpSettings = HttpSettings
    { hostS :: String
    , portS :: Int
    } deriving (Show)

class HasKafkaConsumerSettings env where
  kafkaSettingsL :: Lens' env KafkaConsumerSettings
instance HasKafkaConsumerSettings KafkaConsumerSettings where
  kafkaSettingsL = id
instance HasKafkaConsumerSettings AppSettings where
  kafkaSettingsL = lens getKafkaSettings (\x y -> x { getKafkaSettings = y })

class HasHttpSettings env where
  httpSettingsL :: Lens' env HttpSettings
instance HasHttpSettings HttpSettings where
  httpSettingsL = id
instance HasHttpSettings AppSettings where
  httpSettingsL = lens getHttpSettings (\x y -> x { getHttpSettings = y })