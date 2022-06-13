module Resolver.Settings
  ( HttpServerSettings(..)
  , AppSettings(..)
  , PoolStoreSettings(..)
  , loadAppSettings
  ) where

import RIO

import qualified Data.Text as T

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
  } deriving (Generic, FromDhall)

data HttpServerSettings = HttpServerSettings
  { getPort :: Natural
  , getHost :: String
  } deriving (Generic, FromDhall)

data PoolStoreSettings = PoolStoreSettings
  { storePath       :: FilePath
  , createIfMissing :: Bool
  }
  deriving (Generic, FromDhall)

loadAppSettings :: MonadIO f => Maybe String -> f AppSettings
loadAppSettings maybePath = liftIO $ input auto path
  where path = T.pack $ fromMaybe "./pools-resolver/resources/config.dhall" maybePath
