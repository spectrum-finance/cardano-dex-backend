let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{
  ledgerSyncConfig =
    { nodeSocketPath = "/root/cardano-node/ipc/node.socket"
    , maxInFlight = 256
    , startAt =
      { slot = 59190535
      , hash = "2ba0e86679ddd0015ee5a4f17c093201a2b5e41ca919152af89333b5c8964319"
      }
    }
, nodeConfigPath = "/root/cardano-node/node-conf.json"
, loggingConfig =
    { rootLogLevel   = LogLevel.Info
    , fileHandlers   = [fileHandlers "Path" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    }
}