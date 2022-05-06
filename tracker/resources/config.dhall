let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{ explorerConfig =
    { explorerUri = "https://testnet-api.quickblue.io"
    },
  blockRequestConfig =
    { period = 1
    },
  poolsProducerConfig =
    { producerBrokers = ["127.0.0.1:9092"]
    , producerTimeout = 1000
    },
  poolsTopicName = "pools-topic",
  ordersProducerConfig =
    { producerBrokers = ["127.0.0.1:9092"]
    , producerTimeout = 1000
    },
  ordersTopicName = "orders-topic",
  trackerProgrammConfig =
    { pollTime = 10
    , minIndex = 10190292
    },
  redisConfig =
    { redisHost = "0.0.0.0"
    , redisPort = "6379"
    , redisPassword = Some "redis_ergo_password"
    },
  trackerServiceConfig =
    { limitOffset = 1000
    , maxAttempts = 5
    },
  loggingConfig =
    { fileHandlers = [fileHandlers "./logs/tracker.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    }
}