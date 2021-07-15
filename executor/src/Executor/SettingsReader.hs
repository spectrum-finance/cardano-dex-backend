module Executor.SettingsReader where

import RIO
import Executor.Models.Settings

readSettings :: IO AppSettings
readSettings = do
    let kafkaS = KafkaConsumerSettings ["0.0.0.0:9000"] "resolver_group_id_1" ["amm-topic"] 1000
        httpS  = HttpSettings "0.0.0.0" 8081
    pure $ AppSettings kafkaS httpS