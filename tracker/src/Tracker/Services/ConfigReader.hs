module Tracker.Services.ConfigReader 
  ( ConfigReader(..)
  , mkConfigReader
  ) where

import Tracker.Models.AppConfig

import Prelude
import Dhall
import Control.Monad.IO.Class

data ConfigReader f = ConfigReader
  { read :: f AppConfig 
  }

mkConfigReader :: (MonadIO f) => ConfigReader f
mkConfigReader = ConfigReader $ liftIO $ input auto "./tracker/resources/config.dhall"