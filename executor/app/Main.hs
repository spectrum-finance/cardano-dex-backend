module Main where

import Executor.SettingsReader
import Executor.HttpClient
import Executor.Sender
import Executor.Processor
import Dex.Interpreter
import Executor.KafkaClient
import RIO

main :: IO ()
main = do
    appSettings <- readSettings
    runRIO appSettings $ do
        httpClient <- mkHttpClient
        let sender = mkSenderService
            interpreter = mkInterpreterService
            processor = mkProcessor sender httpClient interpreter
            kafkaClient = mkKafkaConsumerS processor
        runKafka kafkaClient

     
