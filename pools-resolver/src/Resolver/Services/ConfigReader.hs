module Resolver.Services.ConfigReader
    ( ConfigReader(..)
    , mkConfigReader
    ) where

import Resolver.Models.AppSettings
import RIO
import Dhall

data ConfigReader f = ConfigReader
  { read :: f AppSettings
  }

mkConfigReader :: (MonadIO f) => ConfigReader f
mkConfigReader = ConfigReader $ liftIO $ input auto "./pools-resolver/resources/config.dhall"
