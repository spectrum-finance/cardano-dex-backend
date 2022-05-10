module Resolver.AppWiring
  ( App(..)
  , mkApp
  ) where

import RIO

import Control.Monad.Trans.Resource (ResourceT, ResIO)
import Control.Concurrent           (forkIO)

import System.Logging.Hlog (makeLogging, Logging(Logging, infoM), MakeLogging(forComponent))

import Resolver.Endpoints.HttpServer
import Resolver.Repositories.PoolRepository
import Resolver.Program.Resolver
import Resolver.Services.PoolResolver
import Resolver.Settings (AppSettings(..))
import Streaming.Consumer

newtype App m = App { runApp :: m () }

mkApp
  :: UnliftIO IO
  -> AppSettings
  -> ResIO (App IO)
mkApp ul AppSettings{..} = do
  mkLogging       <- makeLogging @(ResourceT IO) @IO loggingConfig
  poolRepository  <- mkPoolRepository poolStoreSettings mkLogging
  consumer        <- mkKafkaConsumer kafkaConfig [topicId]
  poolResolver    <- mkPoolResolver poolRepository mkLogging
  let
    resolver   = mkResolver poolRepository consumer
    httpServer = mkHttpServer httpSettings poolResolver poolRepository ul
  Logging{infoM} <- forComponent mkLogging "App"
  pure . App $ infoM @String "Starting Faucet App .."
    >> forkIO (run resolver)
    >> runHttpServer httpServer
