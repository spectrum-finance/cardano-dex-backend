module Tracker.Models.AppSettings 
    ( ExplorerSettings(..)
    , ClientSettings(..)
    , BlockRequestSettings(..)
    , KafkaProducerSettings(..)
    , ExplorerProgrammSettings(..)
    , AppSettings(..)
    ) where

import RIO
import Dhall

data ExplorerProgrammSettings = ExplorerProgrammSettings
  { pollTime :: Natural
  } deriving (Generic, Show)

instance FromDhall ExplorerProgrammSettings

data ExplorerSettings = ExplorerSettings
  { limitOffset :: Natural
  } deriving (Generic, Show)

instance FromDhall ExplorerSettings

data ClientSettings = ClientSettings
    { getExplorerHost :: String
    , getExplorerPort :: Natural
    } deriving (Generic, Show)

instance FromDhall ClientSettings

newtype BlockRequestSettings = BlockRequestSettings
    { getPeriod :: Natural } deriving (Generic, Show)

instance FromDhall BlockRequestSettings

data AppSettings = AppSettings
    { getExplorerSettings :: ExplorerSettings
    , getClientSettings :: ClientSettings
    , getBlockRequestSettings :: BlockRequestSettings
    , getKafkaProducerSettings :: KafkaProducerSettings
    , explorerProgrammSettings :: ExplorerProgrammSettings
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