module Resolver.Services.SettingsReader
    ( SettingsReader(..)
    , mkSettingsReader
    ) where

import Resolver.Models.AppSettings
import RIO

data SettingsReader = SettingsReader
    { read :: IO AppSettings
    }

mkSettingsReader :: SettingsReader
mkSettingsReader = SettingsReader read'

read' :: IO AppSettings
read' = do
    let kafkaSettings = KafkaConsumerSettings ["0.0.0.0:9092"] "resolver_group_id_1" ["amm-topic"] 1000
        httpSettings = HttpServerSettings 8082 "0.0.0.0"
        appSettings = AppSettings kafkaSettings httpSettings
    pure appSettings
