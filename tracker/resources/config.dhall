{ explorerConfig =
    { explorerHost = ./explorer.dhall
    , explorerPort = 80
    },
  blockRequestConfig =
    { period = 1
    },
  poolsProducerConfig =
    { producerBrokers = ["kafka:9092"]
    , producerTimeout = 1000
    },
  poolsTopicName = "amm-topic",
  ordersProducerConfig =
    { producerBrokers = ["kafka:9092"]
    , producerTimeout = 1000
    },
  ordersTopicName = "proxy-topic",
  trackerProgrammConfig =
    { pollTime = 1000
    },
  redisConfig =
    { redisHost = "redis"
    , redisPort = "6379"
    },
  trackerServiceConfig =
    { limitOffset = 50
    }
}