module Resolver.Wirings.WiringApp
    ( runApp
    ) where

import RIO
import Resolver.Services.ConfigReader        as CR
import Resolver.Endpoints.HttpServer
import Resolver.Repositories.PoolRepository
import Resolver.Program.Resolver
import Resolver.Services.PoolResolver
import Resolver.Models.AppSettings
import Control.Monad.Trans.Resource
import Streaming.Consumer

runApp :: IO ()
runApp = runResourceT $ do
  AppSettings {..} <- lift $ CR.read CR.mkConfigReader
  _ <- lift $ print "test"
  poolRepository   <- lift $ mkPoolRepository redisSettings
  _ <- lift $ print "test1"
  consumer         <- mkKafkaConsumer kafkaConfig [topicId]
  _ <- lift $ print "test2"
  let resolver      = mkResolver poolRepository consumer
  _ <- lift $ print "test3"
  let poolResolver  = mkPoolResolver poolRepository
  _ <- lift $ print "test4"
  let httpServer    = mkHttpServer httpSettings poolResolver poolRepository (UnliftIO id)
  _ <- lift $ print "test5"
  _ <- lift $ runHttpServer httpServer
  _ <- lift $ print "test6"
  lift $ run resolver