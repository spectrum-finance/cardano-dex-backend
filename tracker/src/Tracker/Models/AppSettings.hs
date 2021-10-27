module Tracker.Models.AppSettings 
    ( ExplorerSettings(..)
    , BlockRequestSettings(..)
    , ExplorerProgrammSettings(..)
    , AppSettings(..)
    ) where

import RIO
import Dhall
import Explorer.Config
import Streaming.Config

data ExplorerProgrammSettings = ExplorerProgrammSettings
  { pollTime :: Natural
  } deriving (Generic, Show)

instance FromDhall ExplorerProgrammSettings

data ExplorerSettings = ExplorerSettings
  { limitOffset :: Natural
  } deriving (Generic, Show)

instance FromDhall ExplorerSettings

newtype BlockRequestSettings = BlockRequestSettings
  { getPeriod :: Natural } deriving (Generic)

instance FromDhall BlockRequestSettings

data AppSettings = AppSettings
  { getExplorerSettings :: ExplorerSettings
  , getExplorerConfig :: ExplorerConfig
  , getBlockRequestSettings :: BlockRequestSettings
  , poolsProducerConfig :: KafkaProducerConfig
  , poolsTopicName :: Text
  , ordersProducerConfig :: KafkaProducerConfig
  , ordersTopicName :: Text
  , explorerProgrammSettings :: ExplorerProgrammSettings
  } deriving (Generic)

instance FromDhall AppSettings