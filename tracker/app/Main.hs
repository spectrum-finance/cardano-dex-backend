{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tracker.TxOutsProcessor
import Tracker.KafkaClient
import Tracker.HttpClient
import Tracker.Models.AppSettings 
    ( HttpSettings(..)
    , BlockRequestSettings(..)
    , KafkaProducerSettings(..)
    , AppSettings(..)
    )
import RIO ( runRIO )
import Dex.Processor

main :: IO ()
main = do
    appSettings <- readSettings
    let txOutsProcessor = mkTxOutsProcessor mkProcessorService mkKafkaProducerClient mkHttpClient
    runRIO appSettings $ do (run txOutsProcessor)

readSettings :: IO AppSettings
readSettings = do
    let httpSs = HttpSettings "0.0.0.0" 8081 
        blockRequestS = BlockRequestSettings 0
        kafkaSs = KafkaProducerSettings "amm-topic" "proxy-topic" ["0.0.0.0:9092"] "default-proxy-key" "default-amm-key" 
    pure $ AppSettings httpSs blockRequestS kafkaSs
