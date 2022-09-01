module Resolver
  ( runApp
  ) where

import RIO

import System.Logging.Hlog (makeLogging, Logging(Logging, infoM), MakeLogging(forComponent))

import Resolver.Endpoints.HttpServer
import Resolver.Repositories.PoolRepository
import Resolver.Program.Resolver
import Resolver.Services.PoolResolver
import Resolver.Settings (AppSettings(..), loadAppSettings)
import Streaming.Consumer
import Control.Monad.Trans.Resource (ResourceT, runResourceT)

runApp :: IO ()
runApp = runResourceT $ do
  AppSettings {..} <- lift $ loadAppSettings Nothing
  let ul = UnliftIO id
  mkLogging        <- makeLogging @(ResourceT IO) @IO loggingConfig
  poolRepository   <- mkPoolRepository poolStoreSettings mkLogging
  consumer         <- mkKafkaConsumer kafkaConfig [topicId]
  poolResolver     <- mkPoolResolver poolRepository mkLogging
  resolver         <- mkResolver poolRepository mkLogging consumer
  let
    httpServer = mkHttpServer httpSettings poolResolver poolRepository ul
  Logging{infoM} <- forComponent mkLogging "App"
  lift $ infoM @String "Starting Resolver App .."
    >> concurrently_ (run resolver) (runHttpServer httpServer) 
