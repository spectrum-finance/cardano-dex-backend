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
import Tracker.Services.Logger as Log

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
  => f()
wire = runResourceT $ do
  AppConfig {..} <- lift $ read mkConfigReader
  poolsProducer    <- mkKafkaProducer poolsProducerConfig (TopicName poolsTopicName)
  ordersProducer   <- mkKafkaProducer ordersProducerConfig (TopicName ordersTopicName)
  _                <- lift $ Log.log "Both producers started successfully"
  trackerCache     <- mkTrackerCache redisConfig
  _                <- lift $ Log.log "trackerCache started successfully"
  let 
    explorer        = mkExplorer explorerConfig
    trackerService  = mkTrackerService trackerServiceConfig trackerCache explorer
    trackerProgramm = mkTrackerProgram trackerProgrammConfig trackerService ordersProducer poolsProducer
  lift $ run trackerProgramm