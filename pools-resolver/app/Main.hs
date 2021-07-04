module Main where

import RIO
import Resolver.KafkaClient 
import Resolver.HttpServer
import Resolver.Models.AppSettings
import Network.Wai.Handler.Warp as B
import Control.Concurrent.ParallelIO
import qualified Streamly.Prelude as S
import Streamly.Prelude

main :: IO ()
main = do
    appSettings <- readSettings
    let httpServer = S.fromEffect $ print "Running http server" >> (B.run 8081 app)
        kafkaKlient = S.fromEffect (runRIO appSettings runKafka)
    runRIO appSettings $ do
        liftIO $ 
            S.parallel httpServer kafkaKlient
                & S.drain

readSettings :: IO AppSettings
readSettings = do
    pure $ AppSettings 1
