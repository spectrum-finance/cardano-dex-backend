{-# LANGUAGE OverloadedStrings #-}

module Main where

import RIO ( runRIO )
import Resolver.KafkaClient 

main :: IO ()
main = runConsumerExample
