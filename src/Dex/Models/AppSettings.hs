module Dex.Models.AppSettings (HttpSettings(..)) where

import RIO.Text
import RIO

data HttpSettings = HttpSettings
    { hostS :: Text
    , portS :: Int
    , networkIdentifierS :: Text
    , blockchainIdentifierS :: Text
    } deriving (Show)