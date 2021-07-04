module Resolver.Models.AppSettings where

import RIO

data KafkaConsumerSettings = KafkaConsumerSettings
  { brokerList :: [String]
  , groupId :: String
  , topicsList :: [String]
  , poolRate :: Int
  }

class HasKafkaConsumerSettings env where
  kafkaSettingsL :: Lens' env KafkaConsumerSettings
instance HasKafkaConsumerSettings KafkaConsumerSettings where
  kafkaSettingsL = id
instance HasKafkaConsumerSettings AppSettings where
  kafkaSettingsL = lens kafkaSettings (\x y -> x { kafkaSettings = y })

data HttpServerSettings = HttpServerSettings
  { port :: Int
  , host :: String
  }

class HasHttpServerSettings env where
  httpSettingsL :: Lens' env HttpServerSettings
instance HasHttpServerSettings HttpServerSettings where
  httpSettingsL = id
instance HasHttpServerSettings AppSettings where
  httpSettingsL = lens httpSettings (\x y -> x { httpSettings = y })

data AppSettings = AppSettings
    { kafkaSettings :: KafkaConsumerSettings
    , httpSettings :: HttpServerSettings
    }

class HasAppSettings env where
  appSettingsL :: Lens' env AppSettings
instance HasAppSettings AppSettings where
  appSettingsL = id