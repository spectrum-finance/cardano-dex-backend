{ getKafkaSettings = 
    { getBrokerList = ["kafka:9092"]
    , getGroupId = "executor_group_id_1"
    , getTopicsList = ["proxy-topic"]
    , getPollRate = 1000
    , getBatchSize = 1
    },
  getHttpSettings = 
    { getHost = "resolver"
    , getPort = 8088
    }
}