{
  kafkaConfig =
    { consumerBrokers = ["127.0.0.1:9092"]
    , consumerGroupId = "resolver_group_id_1"
    , consumerTimeout = 10
    , consumerPollRate = 1000
    , consumerBatchSize = 1
    },
  topicId =
    { unTopicId = "pools-topic"
    },
  httpSettings =
    { getHost = "0.0.0.0"
    , getPort = 8088
    },
  redisSettings =
    { getRedisHost = "127.0.0.1"
    }
}