module Tracker.App(
	App(..),
	mkApp
) where

import Tracker.Programs.TrackerProgram
import Tracker.Services.ExplorerService
import Tracker.Services.KafkaService
import Tracker.Modules.Grabber
import Tracker.Modules.Publisher
import Tracker.Models.AppSettings
import RIO
import Tracker.Services.SettingsReader
import Prelude ( IO(..), return, pure )

data App = App {
	runApp :: IO ()
}

mkApp :: IO App
mkApp = do
  let settingsReader = mkSettingsReader
  appSettings <- read settingsReader
  explorerSer <- mkExplorerService (getExplorerSettings appSettings)
  kafkaService <- mkKafkaService (getKafkaProducerSettings appSettings)
  grabber <- mkGrabber explorerSer
  publisher <- mkPublisher kafkaService
  mkTrackerProgram grabber publisher
  return ( App (pure ()) )