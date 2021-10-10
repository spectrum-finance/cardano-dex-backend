module Tracker.App
  ( App(..)
  , mkApp
  ) where

import Tracker.Programs.TrackerProgram
import Tracker.Services.ExplorerService
import Tracker.Services.KafkaService
import Tracker.Models.AppSettings
import RIO
import Tracker.Services.SettingsReader
import Tracker.Clients.ExplorerClient
import Dex.Processor

data App = App
  { runApp :: IO ()
  }

mkApp :: IO App
mkApp = do
  let settingsReader = mkSettingsReader
  AppSettings {..} <- read settingsReader
  explorerClient <- mkExplorerClient getClientSettings
  explorerService <- mkExplorerService getExplorerSettings explorerClient
  kafkaService <- mkKafkaService getKafkaProducerSettings
  let processorService = mkProcessorService 
  tracker <- mkTrackerProgram processorService explorerService kafkaService 
  return ( App (run tracker) )