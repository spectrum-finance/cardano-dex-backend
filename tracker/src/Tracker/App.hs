module Tracker.App
  ( App(..)
  , mkApp
  ) where

import Tracker.Programs.TrackerProgram
import Tracker.Models.AppSettings
import RIO
import Tracker.Services.SettingsReader
import Streaming.Producer
import Explorer.Service
import Tracker.Services.TrackerService
import Tracker.Repository.ExplorerRepo

import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Streaming.Producer
import Core.Streaming
import ErgoDex.Amm.Pool
import Kafka.Producer
import Control.Monad.Catch
import Streaming.Types
import Control.Monad.Error
import qualified Streamly.Prelude             as S
import Control.Monad.IO.Unlift

data App = App
  { runApp :: IO ()
  }

mkApp :: IO App
mkApp = return $ App $ runResourceT $ do
  AppSettings {..} <- lift $ read mkSettingsReader
  poolsProducer    <- mkKafkaProducer poolsProducerConfig (TopicName poolsTopicName)
  ordersProducer   <- mkKafkaProducer ordersProducerConfig (TopicName ordersTopicName)
  exporerRepo      <- mkExplorerRepo redisConfig
  let 
    explorer        = mkExplorer explorerConfig
    trackerService  = mkTrackerService explorerSettings exporerRepo explorer
    trackerProgramm = mkTrackerProgram explorerProgrammSettings trackerService ordersProducer poolsProducer
  lift $ run trackerProgramm