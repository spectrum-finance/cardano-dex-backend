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

data App = App
  { runApp :: IO ()
  }

mkApp :: IO App
mkApp = do
  let settingsReader  = mkSettingsReader
  AppSettings {..}   <- read settingsReader
  let 
    explorer        = mkExplorer getExplorerConfig
    exporerRepo     = mkExplorerRepo
    trackerService  = mkTrackerService getExplorerSettings exporerRepo explorer
  poolEventsProducer <- mkKafkaProducer getKafkaProducerSettings
  tracker            <- mkTrackerProgram explorerService kafkaService 
  return ( App (run tracker) )