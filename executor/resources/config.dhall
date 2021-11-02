{ kafkaConfig = 
    { consumerBrokers = ["127.0.0.1:9092"]
    , consumerGroupId = "executor_group_id_1"
    , consumerPollRate = 1000
    , consumerBatchSize = 1
    , consumerTimeout = 1000
    },
  topicId = 
    { unTopicId = "proxy-topic"
    },
  poolsResolverConfig = 
    { getHost = "0.0.0.0"
    , getPort = 8088
    },
  paymentConfig =
    { pubKeyHash = ""
    , feeAddr = ""
    }
}