{-# LANGUAGE OverloadedStrings #-}

module Main where

import RIO ( runRIO )
import Resolver.KafkaClient 
import Resolver.Models.AppSettings

main :: IO ()
main = do
    appSettings <- readSettings
    runRIO appSettings $ do run

readSettings :: IO AppSettings
readSettings = do
    pure $ AppSettings 1
