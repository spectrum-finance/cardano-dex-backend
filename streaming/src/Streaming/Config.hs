module Streaming.Config where

import RIO

data KafkaProducerConfig = KafkaProducerConfig
  { brokers :: [Text]
  , timeout :: Int
  }
