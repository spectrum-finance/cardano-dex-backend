-- Configuration of spectrum off-chain service docker image for Cardano network
-- More info: github.com/spectrum-finance/cardano-dex-backend
-- Change it carefully
let FeePolicy = < Strict | Balance >
let CollateralPolicy = < Ignore | Cover >

let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ 
  {-
    Set network mode, possible values True | False.
    Effects on usage of networkConfig.cardanoNetworkId field.
    The mainnet mode does not use networkConfig.cardanoNetworkId
  -}
  mainnetMode = True,
  
  {- 
    Node socket configuration.
    Provide information about node socket path with which the connection is established
    Recommendation: 
    Do not change it
  -}
  nodeSocketConfig =
    { nodeSocketPath = "/ipc/node.socket"
    , maxInFlight    = 256
    },
  
  {- 
    Event source configuration.
    Provide information about start point of syncronizataion process.
    Recommendation: 
    Do not change it
  -}
  eventSourceConfig =
    { startAt =
        { slot = 98823654
        , hash = "4666f26d15f4802c0d4c81b841583ea6d90d623d168c77f1e45200eda1f82638"
        }
    }

  networkConfig =
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

  {- 
    Backlog is a part of app that reponsible for orders execution.
    Configuration below provide main orders parameters:
     * orderLifetime - Time in picoseconds describing how long from 
       current time order will be considered as ready for execution
     * orderExecTime - Time in picoseconds describing how long from 
       current time order will be rechecked for executed status. In
       case if order was not executed - backlog will try to do it
       once again
     * suspendedPropability - Set probability level between execution
       orders with some recoverable exceptions (e.g PriceTooHigh - 
       situation when order couldn't be executed due to slippage
       tollarance) and new orders parsed from network.
       Example:
         - suspendedPropability = 5
           New orders will be execured with propability 95%
  -}
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
    { swapRef = "fc9e99fd12a13a137725da61e57a410e36747d513b965993d92c32c67df9259a#2"
    , depositRef = "fc9e99fd12a13a137725da61e57a410e36747d513b965993d92c32c67df9259a#0"
    , redeemRef = "fc9e99fd12a13a137725da61e57a410e36747d513b965993d92c32c67df9259a#1"
    , poolRef = "31a497ef6b0033e66862546aa2928a1987f8db3b8f93c59febbe0f47b14a83c6#0"
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
, txSubmitConfig =
    { nodeSocketPath = "/ipc/node.socket"
    }

{- Transaction assembly configuration. (Deprecated) -}
, txAssemblyConfig =
    { feePolicy         = FeePolicy.Balance
    , collateralPolicy  = CollateralPolicy.Cover
    , deafultChangeAddr = "bot addr"
    }
, secrets =
    { secretFile = "/etc/wallet1TS.json"
    , keyPass    = "your passwor"
    }
, loggingConfig =
    { rootLogLevel   = LogLevel.Info
    , fileHandlers   = [fileHandlers "/data/amm-executor.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    }
, unsafeEval =
    { unsafeTxFee = +310000
    , exUnits = 145000000
    , exMem = 330000
    }
}