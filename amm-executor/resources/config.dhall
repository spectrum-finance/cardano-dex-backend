-- Configuration of spectrum off-chain service for Cardano network
-- More info: github.com/spectrum-finance/cardano-dex-backend
let FeePolicy = < Strict | Balance >
let CollateralPolicy = < Ignore | Cover >

let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ 
  {-
    Sets network mode, possible values 'are' True | False.
    Affects on usage of networkConfig.cardanoNetworkId field.
    The mainnet mode does not use networkConfig.cardanoNetworkId
  -}
  mainnetMode = False,

  {- 
    Ledger sync configuration.
    Provides information about node socket path to establish connection with
  -}
  nodeSocketConfig =
    { nodeSocketPath = "/var/lib/docker/volumes/cardano-vasil-docker_node-ipc/_data/node.socket"
    , maxInFlight    = 256
    },

  {- 
    Event source configuration.
    Provides information about start point of syncronizataion process.
    Recommendation: 
    To process order as soon as possible check that your node is fully synced and
    slot with hash are corresponding to one of the latest block in chain
  -}
  eventSourceConfig =
    { startAt =
        { slot = 2729633
        , hash = "815dafb374898811dc74069a8df8af7a98a80214203e89acdd6425c2e3db37c7"
        }
    },

  {- 
    Network configuration.
    (Use only if mainnetMode == False, otherwise leave it unchanged)
    Provides information about network magic id
  -}
  networkConfig = 
      { cardanoNetworkId = 2
      },

  {- 
    Ledger store (responsible for persisting blocks) configuration.
    Provides information about ledger store path.
  -}
  ledgerStoreConfig =
    { storePath       = "./data/amm-executor"
    , createIfMissing = True
    },

  {- 
    Main configuration file of connected node.
    Necessary for correct transaction fee estimation
  -}
  nodeConfigPath = "/root/cardano-vasil-docker/config/preview/config.json",

  {- 
    Pool store (responsible for persisting pools) configuration.
    Provides information about pool store path.
  -}
  pstoreConfig =
    { storePath       = "/path"
    , createIfMissing = True
    },

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
  backlogConfig =
    { orderLifetime        = 10
    , orderExecTime        = 10
    , suspendedPropability = 5
    },

  {- 
    Backlog store (responsible for persisting orders) configuration.
    Provides information about backlog store path.
  -}
  backlogStoreConfig =
    { storePath       = "/path"
    , createIfMissing = True
    },

  {- 
    Explorer configuration.
    If you would like change to your own be sure that resources provide
    the same api as github.com/spectrum-finance/cardano-explorer-backend
  -}
  explorerConfig =
    { explorerUri = "https://testnet-api.quickblue.io"
    },

  {- 
    Transaction reference inputs information.
    There is necessary to provide refs for outputs with off-chain services because 
    off-chain service use CIP-31 (https://cips.cardano.org/cips/cip31/). If you 
    don't want to deploy your one outputs do not change this configuration
  -}
  txsInsRefs =
    { swapRef = "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16#2"
    , depositRef = "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16#3"
    , redeemRef = "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16#4"
    , poolRef = "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16#1"
    },

  {- Scripts upls paths configuration. -}
  scriptsConfig =
    { swapScriptPath    = "./scripts/swap.uplc"
    , depositScriptPath = "./scripts/deposut.uplc"
    , redeemScriptPath  = "./scripts/redeem.uplc"
    , poolScriptPath    = "./scripts/pool.uplc"
    },

  {- Transaction assembly configuration. (Deprecated) -}
  txAssemblyConfig =
    { feePolicy         = FeePolicy.Balance
    , collateralPolicy  = CollateralPolicy.Cover
    , deafultChangeAddr = "addr_test1vr007v5nktnksje3gnm4aw4arwrkcl5rvvx4lwa3w8mtzxgf6c2nt"
    },

  {- 
    Bot's wallet configuration.
    Contains path for encrypted container with private key, generated by wallet-helper-app
    and passphrase. Generally this wallet used only for collaterals and rewards
  -}
  secrets =
    { secretFile = "/path/to/secret/file"
    , keyPass    = "pass"
    },

  {- 
    Logging configuration.
    Recommendation: Use Info level to prevent getting huge log files
  -}
  loggingConfig =
    { rootLogLevel   = LogLevel.Info
    , fileHandlers   = [fileHandlers "logs/amm-executor.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    },

  {- 
    Utxo store (responsible for persisting utxos of bot's wallet) configuration.
    Provides information about utxo store path.
  -}
  utxoStoreConfig =
    { utxoStorePath   = "./path/to/utxoStore"
    , createIfMissing = True
    },

  {- 
    Pool actions configuration.
    Provides information about approximately order transation fee
  -}
  poolActionsConfig =
    { safeTxFeeLovalace = +1500000
    }
}