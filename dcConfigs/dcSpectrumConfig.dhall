let FeePolicy = < Strict | Balance >
let CollateralPolicy = < Ignore | Cover >

let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ mainnetMode = False
, nodeSocketConfig =
    { nodeSocketPath = "/ipc/node.socket"
    , maxInFlight    = 256
    }
, eventSourceConfig =
    { startAt =
        { slot = 13260697
        , hash = "886d9344a6c0191a2b118ab32c0277f22ffaccb20bbb97d2bea7c8103d2a45d4"
        }
    }
, networkConfig =
    { cardanoNetworkId = 2
    }
, ledgerStoreConfig =
    { storePath       = "./data/amm-executor"
    , createIfMissing = True
    }
, nodeConfigPath = "/var/cardano/config/config.json"
, pstoreConfig =
    { storePath       = "./data/psStore"
    , createIfMissing = True
    }
, backlogConfig =
    { orderLifetime        = 9000
    , orderExecTime        = 4500
    , suspendedPropability = 5
    }
, backlogStoreConfig =
    { storePath       = "./data/backlogStore"
    , createIfMissing = True
    }
, utxoStoreConfig =
    { utxoStorePath   = "./data/utxoStore"
    , createIfMissing = True
    }
, txsInsRefs =
    { swapRef = "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16#2"
    , depositRef = "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16#3"
    , redeemRef = "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16#4"
    , poolRef = "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16#1"
    }
, scriptsConfig =
    { swapScriptPath    = "/scripts/swap.uplc"
    , depositScriptPath = "/scripts/deposit.uplc"
    , redeemScriptPath  = "/scripts/redeem.uplc"
    , poolScriptPath    = "/scripts/pool.uplc"
    }
, explorerConfig =
    { explorerUri = "https://explorer.spectrum.fi"
    }
, txAssemblyConfig =
    { feePolicy         = FeePolicy.Balance
    , collateralPolicy  = CollateralPolicy.Cover
    , deafultChangeAddr = "addr_test1vr007v5nktnksje3gnm4aw4arwrkcl5rvvx4lwa3w8mtzxgf6c2nt"
    }
, secrets =
    { secretFile = "/etc/wallet1TS.json"
    , keyPass    = "secret"
    }
, loggingConfig =
    { rootLogLevel   = LogLevel.Info
    , fileHandlers   = [fileHandlers "/data/amm-executor.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    }
}