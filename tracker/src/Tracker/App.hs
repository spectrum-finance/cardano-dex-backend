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

data App = App
  { runApp :: IO ()
  }

mkApp :: IO App
mkApp = return $ App $ liftIO wiring

wiring
  :: (MonadIO f, MonadUnliftIO f, MonadCatch f, MonadError ProducerExecption f, MonadError KafkaError f, S.MonadAsync f)
  => f ()
wiring = runResourceT $ do
  AppSettings {..} <- lift $ read mkSettingsReader
  poolsProducer    <- mkKafkaProducer poolsProducerConfig (TopicName poolsTopicName) -- :: ResourceT IO (Producer IO PoolId ConfirmedPoolEvent)
  ordersProducer   <- mkKafkaProducer ordersProducerConfig (TopicName ordersTopicName) -- :: ResourceT IO (Producer IO PoolId ConfirmedOrderEvent)
  let 
    explorer        = mkExplorer getExplorerConfig
    exporerRepo     = mkExplorerRepo
    trackerService  = mkTrackerService getExplorerSettings exporerRepo explorer
    trackerProgramm = mkTrackerProgram explorerProgrammSettings trackerService ordersProducer poolsProducer
  lift $ run trackerProgramm