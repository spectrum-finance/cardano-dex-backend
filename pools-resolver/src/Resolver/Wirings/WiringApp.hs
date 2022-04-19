module Resolver.Wirings.WiringApp
    ( runApp
    ) where

import RIO
import Resolver.Services.ConfigReader        as CR
import Resolver.Endpoints.HttpServer
import System.Logging.Hlog
import Resolver.Repositories.PoolRepository
import Resolver.Program.Resolver
import Resolver.Services.PoolResolver
import Resolver.Models.AppSettings
import Control.Monad.Trans.Resource
import Streaming.Consumer

runApp :: IO ()
runApp = runResourceT $ do
  AppSettings {..} <- lift $ CR.read CR.mkConfigReader
  loggingMaker     <- makeLogging @(ResourceT IO) @IO loggingConfig
  poolRepository   <- lift $ mkPoolRepository redisSettings
  consumer         <- mkKafkaConsumer kafkaConfig [topicId]
  poolResolver     <- mkPoolResolver poolRepository loggingMaker
  let
    resolver      = mkResolver poolRepository consumer
    httpServer    = mkHttpServer httpSettings poolResolver poolRepository (UnliftIO id)
  lift $ concurrently_ (runHttpServer httpServer) (run resolver) 