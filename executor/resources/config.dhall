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
  nodeConfig =
    { host = "0.0.0.0"
    , port = 1234
    },
  txAssemblyConfig =
    { feePolicy         = FeePolicy.Balance
    , collateralPolicy  = CollateralPolicy.Cover
    , deafultChangeAddr = "addr_test1vr007v5nktnksje3gnm4aw4arwrkcl5rvvx4lwa3w8mtzxgf6c2nt"
    },
  secretFile =
    { unSigningKeyFile = "/Users/aleksandr/test123/cardano-dex-sdk-haskell/test/ts.json",
    },
  explorerConfig =
    { explorerHost = "0.0.0.0"
    , explorerPort = 8084
    },
  keyPass =
    { unKeyPass = "secret"
    },
  topicId = 
    { unTopicId = "orders-topic"
    },
  poolsResolverConfig = 
    { getHost = "0.0.0.0"
    , getPort = 8088
    },
  paymentConfig =
    { pubKeyHash = "d74d26c5029cf290094fce1a0670da7369b9026571dfb977c6fa234f,"
    , feeAddr = ""
    }
}