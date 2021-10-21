module Streaming.Class where

import Kafka.Producer

class ToKafka k v where
  toKafka :: TopicName -> k -> v -> ProducerRecord
