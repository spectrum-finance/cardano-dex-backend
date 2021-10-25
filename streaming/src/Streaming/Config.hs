module Streaming.Config where

import RIO

data KafkaProducerConfig = KafkaProducerConfig
  { producerBrokers :: [Text]
  , producerTimeout :: Int
  }

data KafkaConsumerConfig = KafkaConsumerConfig
  { consumerBrokers   :: [Text]
  , consumerGroupId   :: Text
  , consumerPollRate  :: Natural
  , consumerBatchSize :: Natural
  , consumerTimeout   :: Natural
  }
