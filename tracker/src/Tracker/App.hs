module Tracker.App
  ( App(..)
  , mkApp
  ) where

import Streaming.Producer

import Explorer.Service

import Tracker.Programs.TrackerProgram
import Tracker.Models.AppConfig
import Tracker.Services.ConfigReader
import Tracker.Services.TrackerService
import Tracker.Caches.TrackerCache

import           System.Logging.Hlog
import           RIO
import qualified Streamly.Prelude as S
import           Control.Monad.Trans.Resource
import           Control.Monad.Catch
import           Kafka.Producer

data App = App
  { runApp :: IO ()
  }

mkApp :: IO App
mkApp = return $ App wire

wire
  :: (MonadUnliftIO f, MonadCatch f, S.MonadAsync f)
  => f ()
wire = runResourceT $ do
  AppConfig {..} <- lift $ read mkConfigReader
  loggingMaker   <- makeLogging loggingConfig
  poolsProducer    <- mkKafkaProducer poolsProducerConfig (TopicName poolsTopicName)
  ordersProducer   <- mkKafkaProducer ordersProducerConfig (TopicName ordersTopicName)
  trackerCache     <- mkTrackerCache redisConfig trackerProgrammConfig
  let
    explorer        = mkExplorer explorerConfig
  trackerService  <- mkTrackerService trackerServiceConfig loggingMaker trackerCache explorer
  trackerProgramm <- mkTrackerProgram trackerProgrammConfig loggingMaker trackerService ordersProducer poolsProducer
  lift $ run trackerProgramm