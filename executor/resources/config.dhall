{ getKafkaSettings = 
    { getBrokerList = ["127.0.0.1:9092"]
    , getGroupId = "executor_group_id_1"
    , getTopicsList = ["proxy-topic"]
    , getPollRate = 1000
    , getBatchSize = 1
    },
  getHttpSettings = 
    { hostS = "0.0.0.0"
    , portS = 8088
    }
}