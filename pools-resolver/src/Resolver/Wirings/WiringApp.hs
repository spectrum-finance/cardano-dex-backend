module Resolver.Wirings.WiringApp
  ( runApp
  ) where

import RIO

import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Concurrent (forkIO)

import System.Logging.Hlog (makeLogging)

import Resolver.Services.ConfigReader as CR
import Resolver.Endpoints.HttpServer
import Resolver.Repositories.PoolRepository
import Resolver.Program.Resolver
import Resolver.Services.PoolResolver
import Resolver.Models.AppSettings
import Streaming.Consumer

runApp :: IO ()
runApp = runResourceT $ do
  AppSettings{..} <- lift $ CR.read CR.mkConfigReader
  mkLogging       <- makeLogging @(ResourceT IO) @IO loggingConfig
  poolRepository  <- mkPoolRepository poolStoreSettings mkLogging
  consumer        <- mkKafkaConsumer kafkaConfig [topicId]
  poolResolver    <- mkPoolResolver poolRepository mkLogging
  let
    resolver   = mkResolver poolRepository consumer
    httpServer = mkHttpServer httpSettings poolResolver poolRepository (UnliftIO id)
  lift $ forkIO (run resolver) >> runHttpServer httpServer
