module Tracker.Models.AppSettings 
    ( ExplorerSettings(..)
    , BlockRequestSettings(..)
    , KafkaProducerSettings(..)
    , ExplorerProgrammSettings(..)
    , AppSettings(..)
    ) where

import RIO
import Dhall
import Explorer.Config

data ExplorerProgrammSettings = ExplorerProgrammSettings
  { pollTime :: Natural
  } deriving (Generic, Show)

instance FromDhall ExplorerProgrammSettings

data ExplorerSettings = ExplorerSettings
  { limitOffset :: Natural
  } deriving (Generic, Show)

instance FromDhall ExplorerSettings

newtype BlockRequestSettings = BlockRequestSettings
    { getPeriod :: Natural } deriving (Generic, Show)

instance FromDhall BlockRequestSettings

data AppSettings = AppSettings
  { getExplorerSettings :: ExplorerSettings
  , getExplorerConfig :: ExplorerConfig
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