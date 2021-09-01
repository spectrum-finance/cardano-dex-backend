module Executor.Services.SettingsReader
    ( SettingsReader(..)
    , mkSettingsReader
    ) where

import RIO
import Executor.Models.Settings
import Dhall

data SettingsReader = SettingsReader
    { read :: IO AppSettings
    }

mkSettingsReader :: SettingsReader
mkSettingsReader = SettingsReader $ input auto "./executor/resources/config.dhall"