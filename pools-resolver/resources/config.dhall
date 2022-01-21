{
  kafkaConfig =
    { consumerBrokers = ["localhost:9092"]
    , consumerGroupId = "resolver_group_id_1"
    , consumerTimeout = 1
    , consumerPollRate = 1
    , consumerBatchSize = 1
    },
  topicId =
    { unTopicId = "amm-topic"
    },
  httpSettings =
    { getHost = "127.0.0.1"
    , getPort = 1015
    },
  redisSettings =
    { getRedisHost = "127.0.0.1"
    }
}