module Tracker.Services.SettingsReader 
    ( SettingsReader(..)
    , mkSettingsReader
    ) where

import RIO
import Tracker.Models.AppSettings
import Dhall

data SettingsReader = SettingsReader
    { read :: IO AppSettings 
    }

mkSettingsReader :: SettingsReader
mkSettingsReader = SettingsReader $ input auto "./tracker/resources/config.dhall"