module Streaming.Class where

import RIO
import Kafka.Producer
import Kafka.Consumer
import Data.ByteString

class ToKafka k v where
  toKafka :: TopicName -> k -> v -> ProducerRecord

class FromKafka k v where
  fromKafka :: ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> Maybe (k, v)
