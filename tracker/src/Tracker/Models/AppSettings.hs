module Tracker.Models.AppSettings 
    ( HttpSettings(..)
    , BlockRequestSettings(..)
    , KafkaProducerSettings(..)
    , AppSettings(..)
    , HasHttpSettings(..)
    , HasBlockRequestSettings(..)
    , HasKafkaProducerSettings(..)
    , HasAppSettings(..)
    ) where

import RIO ( Show, Int, id, lens, String, Lens' )
import RIO.ByteString as BS
import Kafka.Producer
import Dhall

data HttpSettings = HttpSettings
    { getHost :: String
    , getPort :: Natural
    } deriving (Generic, Show)

instance FromDhall HttpSettings

newtype BlockRequestSettings = BlockRequestSettings
    { getPeriod :: Natural } deriving (Generic, Show)

instance FromDhall BlockRequestSettings

data AppSettings = AppSettings
    { getHttpSettings :: HttpSettings
    , getBlockRequestSettings :: BlockRequestSettings
    , getKafkaProducerSettings :: KafkaProducerSettings
    } deriving (Generic, Show)

instance FromDhall AppSettings

data KafkaProducerSettings = KafkaProducerSettings 
  { getAmmTopic :: Text
  , getProxyTopic :: Text
  , getBrokersList :: [Text]
  , getProxyMsgKey :: Text
  , getAmmMsgKey :: Text
  } deriving (Generic, Show)

instance FromDhall KafkaProducerSettings

class HasHttpSettings env where
  httpSettingsL :: Lens' env HttpSettings
instance HasHttpSettings HttpSettings where
  httpSettingsL = id
instance HasHttpSettings AppSettings where
  httpSettingsL = lens getHttpSettings (\x y -> x { getHttpSettings = y })

class HasBlockRequestSettings env where
  blockRequestSettingsL :: Lens' env BlockRequestSettings
instance HasBlockRequestSettings BlockRequestSettings where
  blockRequestSettingsL = id
instance HasBlockRequestSettings AppSettings where
  blockRequestSettingsL = lens getBlockRequestSettings (\x y -> x { getBlockRequestSettings = y })

class HasAppSettings env where
  appSettingsL :: Lens' env AppSettings
instance HasAppSettings AppSettings where
  appSettingsL = id

class HasKafkaProducerSettings env where
  kafkaProducerSettingsL :: Lens' env KafkaProducerSettings
instance HasKafkaProducerSettings KafkaProducerSettings where
  kafkaProducerSettingsL = id
instance HasKafkaProducerSettings AppSettings where
  kafkaProducerSettingsL = lens getKafkaProducerSettings (\x y -> x { getKafkaProducerSettings = y })