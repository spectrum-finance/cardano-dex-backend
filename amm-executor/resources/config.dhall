let FeePolicy = < Strict | Balance >
let CollateralPolicy = < Ignore | Cover >
let Network = < Mainnet | Preview >

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
  mainnetMode = True,

  {- 
    Ledger sync configuration.
    Provides information about node socket path to establish connection with
  -}
  nodeSocketConfig =
    { nodeSocketPath = "./path/to/node.socket"
    , maxInFlight    = 256
    },

  {- 
    Event source configuration.
    Provides information about about the starting point of the synchronization process.
    Recommendation:
    To process orders as soon as possible, it is recommended to ensure that your node
    is fully synced and that the slot with the corresponding hash corresponds
    to one of the latest blocks in the chain.
  -}
  eventSourceConfig =
    { startAt =
        { slot = 98823654
        , hash = "4666f26d15f4802c0d4c81b841583ea6d90d623d168c77f1e45200eda1f82638"
        }
    },

  {- 
    Network configuration.

    This configuration provides details about the network magic ID.
    The network magic ID is a unique identifier specific to the network you are connected to.
    It plays a crucial role in ensuring compatibility and synchronization between nodes.

    Please note that this information is applicable only when the mainnetMode is set to False.
    In mainnet mode, the network magic ID is not utilized.
  -}
  networkConfig = 
      { cardanoNetworkId = 2
      },

  {- 
    Ledger store configuration.

    The ledger store configuration manages the persistence of blocks in the system.
    It is responsible for storing blocks in a durable manner.

    The ledger store path specifies the directory or file path where the blocks are persisted.
  -}
  ledgerStoreConfig =
    { storePath       = "./data/amm-executor"
    , createIfMissing = True
    },

  {- 
    The main configuration file of the connected node is crucial for obtaining important
    information about network parameters. It serves as a central source for accessing
    various network-related settings and details.
  -}
  nodeConfigPath = "./config/mainnet/mainnet-config.json",

  {- 
    The pool store configuration manages the persistence of pools in the system.
    It is responsible for storing and managing information related to pools,
    including pool updates.
    This configuration provides details about the pool store path, which specifies
    the location where the pool-related data is stored on the system.
  -}
  pstoreConfig =
    { storePath       = "/path"
    , createIfMissing = True
    },

  {- 
   The backlog is an essential component of the application responsible for executing orders.
   The configuration provided below specifies the main parameters related to order execution:
     * orderLifetime - This parameter defines the duration in picoseconds during which an
        order will be considered ready for execution, starting from the current time.
     * orderExecTime - This parameter determines the duration in picoseconds for rechecking
        the execution status of an order. If an order was not executed within this timeframe,
        the backlog will attempt to execute it again.
     * suspendedPropability - This parameter sets the probability level for executing orders
        with recoverable exceptions, such as situations where the order couldn't be executed
        due to slippage tolerance, known as PriceTooHigh error. The suspendedProbability
        value represents the probability (in percentage) for executing new orders parsed from
        the network.
       Example:
         - If suspendedProbability is set to 5, new orders will be executed with a probability
           of 95%.
  -}
  backlogConfig =
    { orderLifetime        = 4500
    , orderExecTime        = 1500
    , suspendedPropability = 0
    },

  {- 
    The backlog store configuration is responsible for persisting orders in the system.
    It ensures that orders are stored and managed reliably.

    This configuration provides details about the backlog store path,
    which specifies the location where the order-related data is stored on the system.
  -}
  backlogStoreConfig =
    { storePath       = "/path"
    , createIfMissing = True
    },

  {- 
    Explorer configuration.

    It is important to note that the API compatibility is crucial for seamless integration
    of the explorer. Be ensure that the resources you utilize offer the same API endpoints
    as Spectrum-finance explorer, if you want to change original uri
  -}
  explorerConfig =
    { explorerUri = "https://explorer.spectrum.fi"
    , network = Network.Mainnet
    },

  {-
    Transaction reference inputs provide essential information regarding references to outputs
     when using off-chain services. These references are necessary because off-chain service
     adhere to the CIP-31 standard (Cardano Improvement Proposal 31, available at
     https://cips.cardano.org/cips/cip31/).

     If you do not intend to deploy your own outputs, it is recommended not to modify this
     configuration.
  -}
  txsInsRefs =
    { swapRef = "fc9e99fd12a13a137725da61e57a410e36747d513b965993d92c32c67df9259a#2"
    , depositRef = "fc9e99fd12a13a137725da61e57a410e36747d513b965993d92c32c67df9259a#0"
    , redeemRef = "fc9e99fd12a13a137725da61e57a410e36747d513b965993d92c32c67df9259a#1"
    , poolRef = "31a497ef6b0033e66862546aa2928a1987f8db3b8f93c59febbe0f47b14a83c6#0"
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
    , deafultChangeAddr = "bot address"
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
    This configuration provides details about the UTXO store path, which specifies the location
    where the UTXO data of the bot's wallet is stored on the system.
  -}
  utxoStoreConfig =
    { utxoStorePath   = "./path/to/utxoStore"
    , createIfMissing = True
    },

  {- 
    The Pool actions configuration provides information about the approximate transaction fee
    that users, rather than the bot executor, will incur. This fee specifically relates
    to the Cardano network transaction fee and is not associated with execution rewards.

    This value was derived from practical experience, so it is advisable not to modify it
    unless you are confident that the actual transaction fee is higher than the value specified
    in this configuration.
  -}
  poolActionsConfig =
    { safeTxFeeLovalace = +300000
    },

  unsafeEval =
    { unsafeTxFee = +310000
    , exUnits = 145000000
    , exMem = 330000
    }
}