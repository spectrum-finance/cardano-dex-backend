module Dex.Models.AppSettings 
    ( HttpSettings(..)
    , BlockRequestSettings(..)
    , AppSettings(..)
    , HasHttpSettings(..)
    , HasBlockRequestSettings(..)
    , HasAppSettings(..)
    ) where

import RIO.Text
import RIO

data HttpSettings = HttpSettings
    { hostS :: String
    , portS :: String
    } deriving (Show)

data BlockRequestSettings = BlockRequestSettings
    { period :: Int } deriving (Show)

data AppSettings = AppSettings
    { httpSettings :: HttpSettings
    , blockRequestSettings :: BlockRequestSettings
    } deriving (Show)

class HasHttpSettings env where
  httpSettingsL :: Lens' env HttpSettings
instance HasHttpSettings HttpSettings where
  httpSettingsL = id
instance HasHttpSettings AppSettings where
  httpSettingsL = lens httpSettings (\x y -> x { httpSettings = y })

class HasBlockRequestSettings env where
  blockRequestSettingsL :: Lens' env BlockRequestSettings
instance HasBlockRequestSettings BlockRequestSettings where
  blockRequestSettingsL = id
instance HasBlockRequestSettings AppSettings where
  blockRequestSettingsL = lens blockRequestSettings (\x y -> x { blockRequestSettings = y })

class HasAppSettings env where
  appSettingsL :: Lens' env AppSettings
instance HasAppSettings AppSettings where
  appSettingsL = id