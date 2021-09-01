{ getHttpSettings = 
    { getHost = "explorer"
    , getPort = 8089
    },
getKafkaProducerSettings = 
    { getBrokersList = ["kafka:9092"]
    , getAmmTopic = "amm-topic"
    , getProxyTopic = "proxy-topic"
    , getProxyMsgKey = "default-proxy-key"
    , getAmmMsgKey = "default-amm-key" 
    },
  getBlockRequestSettings =
    { getPeriod = 0
    }
}