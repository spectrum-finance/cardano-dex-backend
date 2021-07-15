module Resolver.SettingsReader
    ( loadSettings
    ) where

import Resolver.Models.AppSettings
import RIO

loadSettings :: IO AppSettings
loadSettings = do
    let kafkaSettingsR = KafkaConsumerSettings ["0.0.0.0:9000"] "resolver_group_id_1" ["amm-topic"] 1000
        httpSettingsR = HttpServerSettings 8081 "0.0.0.0"
        appSettingsL = AppSettings kafkaSettingsR httpSettingsR
    pure appSettingsL
