module Main where

import RIO
import Resolver.KafkaClient 
import Resolver.HttpServer
import Resolver.Models.AppSettings
import Network.Wai.Handler.Warp as B
import Control.Concurrent.ParallelIO

main :: IO ()
main = do
    appSettings <- readSettings
    runRIO appSettings $ do
        liftIO $ parallel_ [print "Running http server" >> (B.run 8080 app), runRIO appSettings runKafka]

readSettings :: IO AppSettings
readSettings = do
    pure $ AppSettings 1
