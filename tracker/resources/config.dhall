{ explorerConfig =
    { explorerHost = "0.0.0.0"
    , explorerPort = 8084
    },
  blockRequestConfig =
    { period = 1
    },
  poolsProducerConfig =
    { producerBrokers = ["127.0.0.1:9092"]
    , producerTimeout = 1000
    },
  poolsTopicName = "pools-topic",
  ordersProducerConfig =
    { producerBrokers = ["127.0.0.1:9092"]
    , producerTimeout = 1000
    },
  ordersTopicName = "orders-topic",
  trackerProgrammConfig =
    { pollTime = 10
    , minIndex = 10190292
    },
  redisConfig =
    { redisHost = "0.0.0.0"
    , redisPort = "6379"
    },
  trackerServiceConfig =
    { limitOffset = 10
    }
}