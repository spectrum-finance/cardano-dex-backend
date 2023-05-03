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
        { slot = 15414823 
        , hash = "9b39a052d1caad4c342ebc9ac1c7a177c9bd139a798d68aff145e953594235b7"
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
    , persistent = False
    }
, backlogConfig =
    { orderLifetime        = 9000
    , orderExecTime        = 4500
    , suspendedPropability = 5
    }
, backlogStoreConfig =
    { storePath       = "./backlogStore"
    , createIfMissing = True
    , persistent = False
    }
, txsInsRefs =
    { swapRef = "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16#2"
    , depositRef = "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16#3"
    , redeemRef = "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16#4"
    , poolRef = "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16#1"
    }
, scriptsConfig =
    { swapScriptPath    = "./scripts/swap.uplc"
    , depositScriptPath = "./scripts/deposit.uplc"
    , redeemScriptPath  = "./scripts/redeem.uplc"
    , poolScriptPath    = "./scripts/pool.uplc"
    }
, explorerConfig =
    { explorerUri = "https://explorer.spectrum.fi"
    }
, txSubmitConfig =
    { nodeSocketPath = "/home/bromel/projects/cardano-node/ipc/node.socket"
    }
, txAssemblyConfig =
    { feePolicy         = FeePolicy.Balance
    , collateralPolicy  = CollateralPolicy.Cover
    , deafultChangeAddr = "addr_test1vr007v5nktnksje3gnm4aw4arwrkcl5rvvx4lwa3w8mtzxgf6c2nt"
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
, utxoStoreConfig =
    { utxoStorePath = "./utxoStore"
    , createIfMissing   = True
    }
, poolActionsConfig =
    { safeTxFeeLovalace = +1500000
    }
}