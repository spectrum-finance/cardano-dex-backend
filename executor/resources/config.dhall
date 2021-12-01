{ kafkaConfig = 
    { consumerBrokers = ["kafka:9092"]
    , consumerGroupId = "executor_group_id_1"
    , consumerPollRate = 10000
    , consumerBatchSize = 1
    , consumerTimeout = 1000
    },
  topicId = 
    { unTopicId = "proxy-topic"
    },
  poolsResolverConfig = 
    { getHost = "resolver"
    , getPort = 8088
    },
  paymentConfig =
    { pubKeyHash = ""
    , feeAddr = ""
    }
}