let FeePolicy = < Strict | Balance >
let CollateralPolicy = < Ignore | Cover >

let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ mainnetMode = False
, ledgerSyncConfig =
    { nodeSocketPath = "/root/cardano-node/ipc/node.socket"
    , maxInFlight    = 256
    }
, eventSourceConfig =
    { startAt =
        { slot = 59190535
        , hash = "2ba0e86679ddd0015ee5a4f17c093201a2b5e41ca919152af89333b5c8964319"
        }
    }
, ledgerStoreConfig =
    { storePath       = "./data/amm-executor"
    , createIfMissing = True
    }
, nodeConfigPath = "/root/cardano-node/node-conf.json"
, pstoreConfig =
    { storePath       = "/path"
    , createIfMissing = True
    }
, backlogConfig =
    { orderLifetime        = 10
    , orderExecTime        = 10
    , suspendedPropability = 5
    }
, backlogStoreConfig =
    { storePath       = "/path"
    , createIfMissing = True
    }
, explorerConfig =
    { explorerUri = "https://testnet-api.quickblue.io"
    }
, txSubmitConfig =
    { nodeSocketPath = "/root/cardano-node/ipc/node.socket"
    }
, txAssemblyConfig =
    { feePolicy         = FeePolicy.Balance
    , collateralPolicy  = CollateralPolicy.Cover
    , deafultChangeAddr = "addr_test1vr007v5nktnksje3gnm4aw4arwrkcl5rvvx4lwa3w8mtzxgf6c2nt"
    }
, secrets =
    { secretFile = "/path/to/secret/file"
    , keyPass    = "pass"
    }
, loggingConfig =
    { rootLogLevel   = LogLevel.Info
    , fileHandlers   = [fileHandlers "logs/amm-executor.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    }
}