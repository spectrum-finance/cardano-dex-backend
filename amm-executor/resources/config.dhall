let FeePolicy = < Strict | Balance >
let CollateralPolicy = < Ignore | Cover >

let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ mainnetMode = False
, ledgerSyncConfig =
    { nodeSocketPath = "/var/lib/docker/volumes/cardano-vasil-docker_node-ipc/_data/node.socket"
    , maxInFlight    = 256
    }
, eventSourceConfig =
    { startAt =
        { slot = 2729633
        , hash = "815dafb374898811dc74069a8df8af7a98a80214203e89acdd6425c2e3db37c7"
        }
    }
, networkConfig = 
    { cardanoNetworkId = 2
    }
, ledgerStoreConfig =
    { storePath       = "./data/amm-executor"
    , createIfMissing = True
    }
, nodeConfigPath = "/root/cardano-vasil-docker/config/preview/config.json"
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
    { nodeSocketPath = "/var/lib/docker/volumes/cardano-vasil-docker_node-ipc/_data/node.socket"
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