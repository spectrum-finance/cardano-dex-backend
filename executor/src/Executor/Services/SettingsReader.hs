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
    let kafkaS = KafkaConsumerSettings ["kafka:9092"] "executor_group_id_1" ["proxy-topic"] 1000 1
        httpS  = HttpSettings "resolver" 8088
    pure $ AppSettings kafkaS httpS