let FeePolicy = < Strict | Balance >
let CollateralPolicy = < Ignore | Cover >
in
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
    , getPort = 1015
    },
  paymentConfig =
    { pubKeyHash = ""
    , feeAddr = ""
    },
  explorerConfig =
    { explorerHost = "http://136.243.21.170"
    , explorerPort = 80
    },
  nodeConfig =
    { host = "0.0.0.0"
    , port = 1234
    },
  secretFile =
    { unSigningKeyFile = "someFile"
    },
  keyPass =
    { unKeyPass = "pass"
    },
  txAssemblyConfig =
    { feePolicy = FeePolicy.Balance
    , collateralPolicy = CollateralPolicy.Cover
    }
}