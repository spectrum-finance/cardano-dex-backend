let FeePolicy = < Strict | Balance >
let CollateralPolicy = < Ignore | Cover >
let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $pid - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
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
    { unSigningKeyFile = "/Users/aleksandr/IdeaProjects/cardano-dex-backend/executor/ts.json",
    },
  nodeSocketConfig =
    { nodeSocketPath = "/tmp/another.socket"
    },
  explorerConfig =
    { explorerUrl = "https://testnet-api.quickblue.io"
    , exponentialBackoffDelay = 100
    , maxRetries = 5
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
    { pubKeyHash = "deff3293b2e7684b3144f75ebabd1b876c7e83630d5fbbb171f6b119"
    , feeAddr = ""
    },
  loggingConfig =
    { fileHandlers = [fileHandlers "./logs/executor.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : < Info | Error | Warn | Debug > }
    }
}