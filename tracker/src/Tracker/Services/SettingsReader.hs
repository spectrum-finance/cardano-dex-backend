module Tracker.Services.SettingsReader 
    ( SettingsReader(..)
    , mkSettingsReader
    ) where

import RIO
import Tracker.Models.AppSettings

data SettingsReader = SettingsReader
    { read :: IO AppSettings 
    }

mkSettingsReader :: SettingsReader
mkSettingsReader = SettingsReader read'

read' :: IO AppSettings
read' = do
    let httpSettings = HttpSettings "0.0.0.0" 8089 
        reqPeriodSettings = BlockRequestSettings 0
        kafkaSettings = KafkaProducerSettings "amm-topic" "proxy-topic" ["kafka:9092"] "default-proxy-key" "default-amm-key" 
    pure $ AppSettings httpSettings reqPeriodSettings kafkaSettings