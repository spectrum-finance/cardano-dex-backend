module Executor.Models.Settings 
    ( KafkaConsumerSettings(..)
    , HttpSettings(..)
    , HasKafkaConsumerSettings(..)
    , HasHttpSettings(..)
    ) where

import RIO
import Kafka.Consumer

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

class HasHttpSettings env where
  httpSettingsL :: Lens' env HttpSettings
instance HasHttpSettings HttpSettings where
  httpSettingsL = id