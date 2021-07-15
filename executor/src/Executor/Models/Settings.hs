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
  { kafkaConsumerS :: KafkaConsumerSettings
  , httpClientS :: HttpSettings
  }

data KafkaConsumerSettings = KafkaConsumerSettings
  { brokerListS :: [BrokerAddress]
  , groupIdS :: ConsumerGroupId
  , topicsListS :: [TopicName]
  , pollRateS :: Int
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
  kafkaSettingsL = lens kafkaConsumerS (\x y -> x { kafkaConsumerS = y })

class HasHttpSettings env where
  httpSettingsL :: Lens' env HttpSettings
instance HasHttpSettings HttpSettings where
  httpSettingsL = id
instance HasHttpSettings AppSettings where
  httpSettingsL = lens httpClientS (\x y -> x { httpClientS = y })