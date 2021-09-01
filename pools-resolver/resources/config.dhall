{ getKafkaSettings = 
    { getBrokerList = ["kafka:9092"]
    , getGroupId = "resolver_group_id_1"
    , getTopicsList = ["amm-topic"]
    , getPollRate = 1000
    , getBatchSize = 1
    },
  getHttpSettings = 
    { getHost = "0.0.0.0"
    , getPort = 8088
    }
}