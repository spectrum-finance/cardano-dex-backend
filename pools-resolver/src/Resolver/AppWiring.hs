module Resolver.AppWiring
  ( mkApp
  ) where

import RIO

import System.Logging.Hlog (makeLogging, Logging(Logging, infoM), MakeLogging(forComponent))

import Resolver.Endpoints.HttpServer
import Resolver.Repositories.PoolRepository
import Resolver.Program.Resolver
import Resolver.Services.PoolResolver
import Resolver.Settings (AppSettings(..))
import Streaming.Consumer
import Control.Monad.Trans.Resource (ResIO)

mkApp
  :: UnliftIO IO
  -> AppSettings
  -> ResIO ()
mkApp ul AppSettings{..} = do
  mkLogging      <- makeLogging loggingConfig
  poolRepository <- mkPoolRepository poolStoreSettings mkLogging
  consumer       <- mkKafkaConsumer kafkaConfig [topicId]
  poolResolver   <- mkPoolResolver poolRepository mkLogging
  let
    resolver   = mkResolver poolRepository consumer
    httpServer = mkHttpServer httpSettings poolResolver poolRepository ul

  Logging{infoM} <- forComponent mkLogging "App"
  lift $ infoM @String "Starting Faucet App .."
    >> concurrently_ (run resolver) (runHttpServer httpServer)
