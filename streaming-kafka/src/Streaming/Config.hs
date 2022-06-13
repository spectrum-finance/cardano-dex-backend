module Streaming.Config where

import GHC.Natural
import GHC.Generics
import Dhall

data KafkaProducerConfig = KafkaProducerConfig
  { producerBrokers :: [Text]
  , producerTimeout :: Natural
  } deriving (Generic)

instance FromDhall KafkaProducerConfig

data KafkaConsumerConfig = KafkaConsumerConfig
  { consumerBrokers   :: [Text]
  , consumerGroupId   :: Text
  , consumerPollRate  :: Natural
  , consumerBatchSize :: Natural
  , consumerTimeout   :: Natural
  , maxPollIntervalMs :: Natural
  } deriving (Generic)

instance FromDhall KafkaConsumerConfig