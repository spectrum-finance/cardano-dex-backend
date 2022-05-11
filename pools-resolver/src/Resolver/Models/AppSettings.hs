module Resolver.Models.AppSettings
  ( HttpServerSettings(..)
  , AppSettings(..)
  , PoolStoreSettings(..)
  ) where

import RIO
import Kafka.Consumer
import System.Logging.Hlog
import Streaming.Config
import Streaming.Types
import Dhall

data AppSettings = AppSettings
  { kafkaConfig       :: KafkaConsumerConfig
  , topicId           :: TopicId
  , httpSettings      :: HttpServerSettings
  , poolStoreSettings :: PoolStoreSettings
  , loggingConfig     :: LoggingConfig
  } deriving (Generic)

instance FromDhall AppSettings

data HttpServerSettings = HttpServerSettings
  { getPort :: Natural
  , getHost :: String
  } deriving (Generic)

instance FromDhall HttpServerSettings

data PoolStoreSettings = PoolStoreSettings
  { storePath       :: FilePath
  , createIfMissing :: Bool
  }
  deriving (Generic, FromDhall)
