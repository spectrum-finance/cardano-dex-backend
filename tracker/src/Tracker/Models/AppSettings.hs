module Tracker.Models.AppSettings 
    ( ExplorerSettings(..)
    , BlockRequestSettings(..)
    , KafkaProducerSettings(..)
    , AppSettings(..)
    , HasExplorerSettings(..)
    , HasBlockRequestSettings(..)
    , HasKafkaProducerSettings(..)
    , HasAppSettings(..)
    ) where

import RIO ( Show, Int, id, lens, String, Lens' )
import RIO.ByteString as BS
import Kafka.Producer
import Dhall

data ExplorerSettings = ExplorerSettings
    { getExplorerHost :: String
    , getExplorerPort :: Natural
    } deriving (Generic, Show)

instance FromDhall ExplorerSettings

newtype BlockRequestSettings = BlockRequestSettings
    { getPeriod :: Natural } deriving (Generic, Show)

instance FromDhall BlockRequestSettings

data AppSettings = AppSettings
    { getExplorerSettings :: ExplorerSettings
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

class HasExplorerSettings env where
  explorerSettingsL :: Lens' env ExplorerSettings
instance HasExplorerSettings ExplorerSettings where
  explorerSettingsL = id
instance HasExplorerSettings AppSettings where
  explorerSettingsL = lens getExplorerSettings (\x y -> x { getExplorerSettings = y })

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