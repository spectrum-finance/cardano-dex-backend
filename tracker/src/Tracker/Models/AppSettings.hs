module Tracker.Models.AppSettings 
    ( RedisSettings(..)
    , BlockRequestSettings(..)
    , ExplorerProgrammSettings(..)
    , ExplorerSettings(..)
    , AppSettings(..)
    ) where

import RIO
import Dhall
import Explorer.Config
import Streaming.Config

newtype ExplorerProgrammSettings = ExplorerProgrammSettings
  { pollTime :: Natural
  } deriving (Generic)

instance FromDhall ExplorerProgrammSettings

newtype BlockRequestSettings = BlockRequestSettings
  { getPeriod :: Natural } deriving (Generic)

instance FromDhall BlockRequestSettings

data RedisSettings = RedisSettings
  { redisHost :: String
  , redisPort :: String 
  } deriving (Generic)

instance FromDhall RedisSettings

newtype ExplorerSettings = ExplorerSettings
  { limitOffset :: Natural
  } deriving (Generic)

instance FromDhall ExplorerSettings

data AppSettings = AppSettings
  { explorerConfig           :: ExplorerConfig
  , blockRequestSettings     :: BlockRequestSettings
  , poolsProducerConfig      :: KafkaProducerConfig
  , poolsTopicName           :: Text
  , ordersProducerConfig     :: KafkaProducerConfig
  , ordersTopicName          :: Text
  , explorerProgrammSettings :: ExplorerProgrammSettings
  , redisConfig              :: RedisSettings
  , explorerSettings         :: ExplorerSettings
  } deriving (Generic)

instance FromDhall AppSettings