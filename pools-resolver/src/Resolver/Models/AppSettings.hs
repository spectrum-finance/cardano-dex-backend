module Resolver.Models.AppSettings where

import RIO

data AppSettings = AppSettings
    { test :: Int }

class HasAppSettings env where
  appSettingsL :: Lens' env AppSettings
instance HasAppSettings AppSettings where
  appSettingsL = id