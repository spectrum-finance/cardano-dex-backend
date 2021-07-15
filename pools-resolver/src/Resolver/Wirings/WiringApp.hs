module Resolver.Wirings.WiringApp
    ( runApp
    ) where

import RIO
import Resolver.Services.KafkaService
import Resolver.Endpoints.HttpServer
import qualified Streamly.Prelude as S
import Resolver.Services.SettingsReader
import Resolver.Services.PoolApi
import Resolver.Services.PoolsResolver 

runApp :: IO ()
runApp = do   
    let settingsReader = mkSettingsReader
    appSettings <- read settingsReader
    poolApi <- mkPoolApi
    let kafkaService = mkKafkaService poolApi
        poolResolver = mkPoolResolver poolApi
        httpServer = mkHttpServer poolResolver poolApi
    runRIO appSettings $ do
        liftIO $ 
            S.parallel (S.fromEffect $ runRIO appSettings (runHttpServer httpServer)) (S.fromEffect $ runRIO appSettings (runKafka kafkaService))
                & S.drain