module Main where

import RIO
import Resolver.KafkaClient 
import Resolver.HttpServer
import Resolver.Models.AppSettings
import qualified Streamly.Prelude as S
import Resolver.SettingsReader
import Resolver.Pool
import Resolver.PoolsResolver

main :: IO ()
main = do
    appSettings <- loadSettings
    poolApi <- mkPoolApi
    let kafkaS = mkKafkaClientService poolApi
        poolResolverS = mkPoolResolver poolApi
        httpServerA = mkHttpServerService poolResolverS poolApi
    runRIO appSettings $ do
        liftIO $ 
            S.parallel (S.fromEffect $ runRIO appSettings (runHttpServer httpServerA)) (S.fromEffect $ runRIO appSettings (runKafka kafkaS))
                & S.drain
