let LogLevel = < Info | Error | Warn | Debug >
let format = "$time - $loggername - $prio - $msg" : Text
let fileHandlers = \(path : Text) -> \(level : LogLevel) -> {_1 = path, _2 = level, _3 = format}
let levelOverride = \(component : Text) -> \(level : LogLevel) -> {_1 = component, _2 = level}
in
{
  kafkaConfig =
    { consumerBrokers   = ["127.0.0.1:9092"]
    , consumerGroupId   = "resolver_group_id_1"
    , consumerTimeout   = 10
    , consumerPollRate  = 1000
    , consumerBatchSize = 1
    }
, topicId =
    { unTopicId = "pools-topic"
    }
, httpSettings =
    { getHost = "0.0.0.0"
    , getPort = 8088
    }
, poolStoreSettings =
    { storePath       = "./data/pools-store"
    , createIfMissing = True
    }
, loggingConfig =
    { fileHandlers   = [fileHandlers "./resolver.log" LogLevel.Info]
    , levelOverrides = [] : List { _1 : Text, _2 : LogLevel }
    }
}