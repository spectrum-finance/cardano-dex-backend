{-# LANGUAGE OverloadedStrings #-}

module Main where

import Tracker.TxOutsProcessor ( run )
import Tracker.Models.AppSettings 
    ( HttpSettings(..)
    , BlockRequestSettings(..)
    , KafkaProducerSettings(..)
    , AppSettings(..)
    )
import RIO ( runRIO )

main :: IO ()
main = do
    appSettings <- readSettings
    runRIO appSettings $ do run

readSettings :: IO AppSettings
readSettings = do
    let httpSs = HttpSettings "0.0.0.0" 8081 
        blockRequestS = BlockRequestSettings 0
        kafkaSs = KafkaProducerSettings "amm-topic-1" "proxy-topic" ["0.0.0.0:9092"] "default-proxy-key" "default-amm-key" 
    pure $ AppSettings httpSs blockRequestS kafkaSs
