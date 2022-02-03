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
    { pubKeyHash = "2f69da3ca1ab89a2fbfd9d7cdae9616957b5bf34403001fb73be9291"
    , feeAddr = "addr_test1qrt56fk9q2w09yqffl8p5pnsmfeknwgzv4calwthcmazxnl0pwy4t9rk9qlzhd49k40p0yxrsm5c5f8puxlxc9hrqj4sy2tu8f"
    },
  explorerConfig =
    { explorerHost = "http://0.0.0.0"
    , explorerPort = 8084
    },
  nodeConfig =
    { host = "0.0.0.0"
    , port = 1234
    },
  secretFile =
    { unSigningKeyFile = "/Users/aleksandr/IdeaProjects/cardano-dex-backend/executor/resources/keys.txt"
    },
  keyPass =
    { unKeyPass = "pass"
    },
  txAssemblyConfig =
    { feePolicy = FeePolicy.Balance
    , collateralPolicy = CollateralPolicy.Cover
    }
}