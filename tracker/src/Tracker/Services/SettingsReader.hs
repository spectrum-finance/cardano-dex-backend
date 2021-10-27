module Tracker.Services.SettingsReader 
  ( SettingsReader(..)
  , mkSettingsReader
  ) where

import Tracker.Models.AppSettings
import Dhall
import Control.Monad.IO.Class
import Prelude

data SettingsReader f = SettingsReader
  { read :: f AppSettings 
  }

mkSettingsReader :: (MonadIO f) => SettingsReader f
mkSettingsReader = SettingsReader $ liftIO $ input auto "./tracker/resources/config.dhall"