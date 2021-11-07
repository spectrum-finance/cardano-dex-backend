module Resolver.Services.SettingsReader
    ( SettingsReader(..)
    , mkSettingsReader
    ) where

import Resolver.Models.AppSettings
import RIO
import Dhall

data SettingsReader = SettingsReader
    { read :: IO AppSettings
    }

mkSettingsReader :: SettingsReader
mkSettingsReader = SettingsReader $ input auto "./pools-resolver/resources/config.dhall"
