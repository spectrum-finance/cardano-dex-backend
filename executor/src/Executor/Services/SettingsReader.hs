module Executor.Services.SettingsReader
    ( SettingsReader(..)
    , mkSettingsReader
    ) where

import RIO
import Executor.Models.Settings

data SettingsReader = SettingsReader
    { read :: IO AppSettings
    }

mkSettingsReader :: SettingsReader
mkSettingsReader = SettingsReader read'

read' :: IO AppSettings
read' = do
    let kafkaS = KafkaConsumerSettings ["0.0.0.0:9000"] "resolver_group_id_1" ["amm-topic"] 1000
        httpS  = HttpSettings "0.0.0.0" 8081
    pure $ AppSettings kafkaS httpS