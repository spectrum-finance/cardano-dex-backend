module Main where

import RIO
import Resolver.KafkaClient 
import Resolver.HttpServer
import Resolver.Models.AppSettings
import qualified Streamly.Prelude as S
import Resolver.SettingsReader

main :: IO ()
main = do
    appSettings <- loadSettings
    runRIO appSettings $ do
        liftIO $ 
            S.parallel (S.fromEffect $ runRIO appSettings runHttpServer) (S.fromEffect $ runRIO appSettings runKafka)
                & S.drain
