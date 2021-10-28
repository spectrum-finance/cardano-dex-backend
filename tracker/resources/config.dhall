{ explorerConfig =
    { explorerHost = "quickblue.io"
    , explorerPort = 9000
    },
  blockRequestConfig =
    { period = 1
    },
  poolsProducerConfig =
    { producerBrokers = ["127.0.0.1:9092"]
    , producerTimeout = 1000
    },
  poolsTopicName = "amm-topic",
  ordersProducerConfig =
    { producerBrokers = ["127.0.0.1:9092"]
    , producerTimeout = 1000
    },
  ordersTopicName = "proxy-topic",
  trackerProgrammConfig =
    { pollTime = 1000
    },
  redisConfig =
    { redisHost = "127.0.0.1"
    , redisPort = "6379"
    },
  trackerServiceConfig =
    { limitOffset = 50
    }
}