module Executor.Wirings.WiringApp
    ( runApp
    ) where

import Executor.Services.SettingsReader
import Executor.Services.HttpReqService
import Executor.Services.Processor
import Executor.Services.KafkaService
import Executor.Services.BashService
import RIO

runApp :: IO ()
runApp = do
    let settingsReader = mkSettingsReader
    appSettings <- read settingsReader
    runRIO appSettings $ do
        httpClient <- mkHttpReqService
        let 
            bashService = mkBashService
            processor = mkProcessor bashService httpClient
            kafkaClient = mkKafkaService processor
        runKafka kafkaClient