{ explorerConfig =
    { explorerHost = "http://127.0.0.1"
    , explorerPort = 8084
    },
  blockRequestConfig =
    { period = 1
    },
  poolsProducerConfig =
    { producerBrokers = ["localhost:9092"]
    , producerTimeout = 1000
    },
  poolsTopicName = "amm-topic",
  ordersProducerConfig =
    { producerBrokers = ["localhost:9092"]
    , producerTimeout = 1000
    },
  ordersTopicName = "proxy-topic",
  trackerProgrammConfig =
    { pollTime = 1000
    },
  redisConfig =
    { redisHost = "0.0.0.0"
    , redisPort = "6379"
    },
  trackerServiceConfig =
    { limitOffset = 50
    }
}