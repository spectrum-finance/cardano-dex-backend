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
    { unSigningKeyFile = "/Users/aleksandr/test123/cardano-dex-sdk-haskell/test/ts.json",
    },
  explorerConfig =
    { explorerHost = "testnet-api.quickblue.io"
    , explorerPort = 443
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
    },
  loggingConfig =
    { fileHandlers = [fileHandlers "Path" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    }
}