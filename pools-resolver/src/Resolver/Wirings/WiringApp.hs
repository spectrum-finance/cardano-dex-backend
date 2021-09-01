module Resolver.Wirings.WiringApp
    ( runApp
    ) where

import RIO
import Resolver.Services.KafkaService
import Resolver.Endpoints.HttpServer
import qualified Streamly.Prelude as S
import Resolver.Services.SettingsReader
import Resolver.Repositories.PoolRepository
import Resolver.Services.PoolsResolver
import Resolver.Models.AppSettings

runApp :: IO ()
runApp = do   
    let settingsReader = mkSettingsReader
    appSettings <- read settingsReader
    repo <- mkPoolRepository $ redisSettings appSettings
    let kafkaService = mkKafkaService repo
        poolResolver = mkPoolResolver repo
        httpServer = mkHttpServer poolResolver repo
    runRIO appSettings $ do
        liftIO $ 
            S.parallel (S.fromEffect $ runRIO appSettings (runHttpServer httpServer)) (S.fromEffect $ runRIO appSettings (runKafka kafkaService))
                & S.drain