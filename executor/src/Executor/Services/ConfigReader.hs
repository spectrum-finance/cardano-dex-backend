module Executor.Services.ConfigReader
  ( ConfigReader(..)
  , mkConfigReader
  ) where

import RIO
import Executor.Models.Config
import Dhall

data ConfigReader f = ConfigReader
  { read :: f AppConfig
  }

mkConfigReader :: (MonadIO f) => ConfigReader f
mkConfigReader = ConfigReader $ liftIO $ input auto "./executor/resources/config.dhall"