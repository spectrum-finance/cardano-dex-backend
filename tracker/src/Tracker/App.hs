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
import Tracker.Repository.ExplorerRepo

import RIO
import Control.Monad.Trans.Resource
import Kafka.Producer

data App = App
  { runApp :: IO ()
  }

mkApp :: IO App
mkApp = return $ App $ runResourceT $ do
  AppConfig {..} <- lift $ read mkConfigReader
  poolsProducer    <- mkKafkaProducer poolsProducerConfig (TopicName poolsTopicName)
  ordersProducer   <- mkKafkaProducer ordersProducerConfig (TopicName ordersTopicName)
  exporerRepo      <- mkExplorerRepo redisConfig
  let 
    explorer        = mkExplorer explorerConfig
    trackerService  = mkTrackerService trackerServiceConfig exporerRepo explorer
    trackerProgramm = mkTrackerProgram trackerProgrammConfig trackerService ordersProducer poolsProducer
  lift $ run trackerProgramm