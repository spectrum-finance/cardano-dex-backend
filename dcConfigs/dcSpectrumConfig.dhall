let FeePolicy = < Strict | Balance >
let CollateralPolicy = < Ignore | Cover >
let Network = < Mainnet | Preview >

let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ mainnetMode = True
, nodeSocketConfig =
    { nodeSocketPath = "/ipc/node.socket"
    , maxInFlight    = 256
    }
, eventSourceConfig =
    { startAt =
        { slot = 98823654
        , hash = "4666f26d15f4802c0d4c81b841583ea6d90d623d168c77f1e45200eda1f82638"
        }
    }
, networkConfig =
    { cardanoNetworkId = 2
    }
, ledgerStoreConfig =
    { storePath       = "./data/amm-executor"
    , createIfMissing = True
    }
, nodeConfigPath = "/config/mainnet-config.json"
, pstoreConfig =
    { storePath       = "./data/psStore"
    , createIfMissing = True
    }
, backlogConfig =
    { orderLifetime        = 4500
    , orderExecTime        = 1500
    , suspendedPropability = 0
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
    { swapRef = "fc9e99fd12a13a137725da61e57a410e36747d513b965993d92c32c67df9259a#2"
    , depositRef = "fc9e99fd12a13a137725da61e57a410e36747d513b965993d92c32c67df9259a#0"
    , redeemRef = "fc9e99fd12a13a137725da61e57a410e36747d513b965993d92c32c67df9259a#1"
    , poolV1Ref = "31a497ef6b0033e66862546aa2928a1987f8db3b8f93c59febbe0f47b14a83c6#0"
    , poolV2Ref = "c8c93656e8bce07fabe2f42d703060b7c71bfa2e48a2956820d1bd81cc936faa#0"
    }
, scriptsConfig =
    { swapScriptPath    = "/scripts/swap.uplc"
    , depositScriptPath = "/scripts/deposit.uplc"
    , redeemScriptPath  = "/scripts/redeem.uplc"
    , poolV1ScriptPath  = "/scripts/poolV1.uplc"
    , poolV2ScriptPath  = "/scripts/poolV2.uplc"
    }
, explorerConfig =
    { explorerUri = "https://explorer.spectrum.fi"
    , network = Network.Mainnet
    }
, txAssemblyConfig =
    { feePolicy         = FeePolicy.Balance
    , collateralPolicy  = CollateralPolicy.Cover
    , deafultChangeAddr = "bot addr"
    }
, secrets =
    { secretFile = "/etc/wallet1TS.json"
    , keyPass    = "your password"
    }
, loggingConfig =
    { rootLogLevel   = LogLevel.Info
    , fileHandlers   = [fileHandlers "/data/amm-executor.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    }
, unsafeEval =
    { unsafeTxFee = +320000
    , exUnits = 165000000
    , exMem = 530000
    }
}