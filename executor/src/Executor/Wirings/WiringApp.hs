module Executor.Wirings.WiringApp
    ( runApp
    ) where

import Executor.Services.SettingsReader
import Executor.Services.HttpReqService
import Executor.Services.Sender
import Executor.Services.Processor
import Dex.Interpreter
import Executor.Services.KafkaService
import RIO

runApp :: IO ()
runApp = do
    let settingsReader = mkSettingsReader
    appSettings <- read settingsReader
    runRIO appSettings $ do
        httpClient <- mkHttpReqService
        let sender = mkSenderService
            interpreter = mkInterpreterService
            processor = mkProcessor sender httpClient interpreter
            kafkaClient = mkKafkaService processor
        runKafka kafkaClient