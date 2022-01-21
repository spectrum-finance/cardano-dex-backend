{ explorerConfig =
    { explorerHost = "http://136.243.21.170"
    , explorerPort = 80
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