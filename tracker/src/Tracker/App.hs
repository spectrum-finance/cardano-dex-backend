module Tracker.App
  ( App(..)
  , mkApp
  ) where

import Tracker.Programs.TrackerProgram
import Tracker.Models.AppSettings
import RIO
import Tracker.Services.SettingsReader
import Streaming.Producer

data App = App
  { runApp :: IO ()
  }

mkApp :: IO App
mkApp = undefined -- do
  -- let settingsReader = mkSettingsReader
  -- AppSettings {..} <- read settingsReader
  -- explorerClient <- mkExplorerClient getClientSettings
  -- explorerService <- mkExplorerService getExplorerSettings explorerClient
  -- poolEventsProducer <- mkKafkaProducer getKafkaProducerSettings
  -- tracker <- mkTrackerProgram explorerService kafkaService 
  -- return ( App (run tracker) )