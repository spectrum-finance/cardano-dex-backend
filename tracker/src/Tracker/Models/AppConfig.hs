module Tracker.Models.AppConfig 
    ( TrackerStoreSettings(..)
    , BlockRequestConfig(..)
    , TrackerProgrammConfig(..)
    , TrackerServiceConfig(..)
    , AppConfig(..)
    ) where

import RIO
import Dhall
import Explorer.Config
import System.Logging.Hlog
import Streaming.Config

data TrackerProgrammConfig = TrackerProgrammConfig
   { pollTime :: Natural
   , minIndex :: Natural
   } deriving (Generic)

instance FromDhall TrackerProgrammConfig

newtype BlockRequestConfig = BlockRequestConfig
  { period :: Natural 
  } deriving (Generic)

instance FromDhall BlockRequestConfig

data TrackerStoreSettings = TrackerStoreSettings
  { storePath       :: FilePath
  , createIfMissing :: Bool
  }
  deriving (Generic, FromDhall)

data TrackerServiceConfig = TrackerServiceConfig
  { limitOffset :: Natural
  } deriving (Generic)

instance FromDhall TrackerServiceConfig

data AppConfig = AppConfig
  { explorerConfig        :: ExplorerConfig
  , blockRequestConfig    :: BlockRequestConfig
  , poolsProducerConfig   :: KafkaProducerConfig
  , poolsTopicName        :: Text
  , ordersProducerConfig  :: KafkaProducerConfig
  , ordersTopicName       :: Text
  , trackerProgrammConfig :: TrackerProgrammConfig
  , trackerStoreConfig    :: TrackerStoreSettings
  , trackerServiceConfig  :: TrackerServiceConfig
  , loggingConfig       :: LoggingConfig
  } deriving (Generic)

instance FromDhall AppConfig