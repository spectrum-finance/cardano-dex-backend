let FeePolicy = < Strict | Balance >
let CollateralPolicy = < Ignore | Cover >

let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ mainnetMode = False
, ledgerSyncConfig =
    { nodeSocketPath = "/home/bromel/projects/cardano-node/ipc/node.socket"
    , maxInFlight    = 256
    }
, eventSourceConfig =
    { startAt =
        { slot = 4312803
        , hash = "ceee48a2f0e2f32474795f0a5ac503c627b3ffe7bac4775cdcef576dba9cd877"
        }
    }
, networkConfig = 
    { cardanoNetworkId = 2
    }
, ledgerStoreConfig =
    { storePath       = "./data/amm-executor"
    , createIfMissing = True
    }
, nodeConfigPath = "/home/bromel/projects/cardano-dex-backend/config/preview/config.json"
, pstoreConfig =
    { storePath       = "./psStore"
    , createIfMissing = True
    }
, backlogConfig =
    { orderLifetime        = 10
    , orderExecTime        = 10
    , suspendedPropability = 0
    }
, backlogStoreConfig =
    { storePath       = "./bsStore"
    , createIfMissing = True
    }
, explorerConfig =
    { explorerUri = "https://testnet-api.quickblue.io"
    }
, txSubmitConfig =
    { nodeSocketPath = "/home/bromel/projects/cardano-node/ipc/node.socket"
    }
, txAssemblyConfig =
    { feePolicy         = FeePolicy.Balance
    , collateralPolicy  = CollateralPolicy.Cover
    , deafultChangeAddr = "addr_test1qz8q8ymsty33sw7mh4s2wxj2pe4mcrlchxxa37z70l348cyjd3dlf08q9usapw5gt5t8cp8lju7wtwqzk5cj0gxaxyss6w8n66"
    }
, secrets =
    { secretFile = "/home/bromel/projects/cardano-dex-backend/wallet1TS.json"
    , keyPass    = "secret"
    }
, loggingConfig =
    { rootLogLevel   = LogLevel.Info
    , fileHandlers   = [fileHandlers "logs/amm-executor.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    }
}