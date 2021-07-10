module Tracker.TxOutsProcessor 
    ( TxOutsProcessor(..)
    , mkTxOutsProcessor
    ) where

import Prelude (print)
import Tracker.HttpClient (HttpClient(..))
import Tracker.Models.AppSettings (AppSettings, HasAppSettings(..))
import RIO
import qualified Streamly.Prelude as S
import Tracker.KafkaClient
import Dex.Processor

data TxOutsProcessor = TxOutsProcessor
    { run :: RIO AppSettings ()
    }

mkTxOutsProcessor :: ProcessorService -> KafkaProducerClient AppSettings -> HttpClient AppSettings -> TxOutsProcessor
mkTxOutsProcessor p k c = TxOutsProcessor $ run' p k c

-- use more convenient way to unlift RIO to IO
run' :: ProcessorService -> KafkaProducerClient AppSettings -> HttpClient AppSettings -> RIO AppSettings ()
run' p k c = do
    heightTvar <- newTVarIO 0
    settings <- view appSettingsL
    liftIO $ 
        S.repeatM (threadDelay 1000000) >> runRIO settings (process p k c heightTvar)
            & S.drain

process :: ProcessorService -> KafkaProducerClient AppSettings -> HttpClient AppSettings -> TVar Int -> RIO AppSettings ()
process ProcessorService{..} KafkaProducerClient{..} HttpClient{..} heightTVar = do
    chainHeight <- getCurrentHeight
    appHeight   <- readTVarIO heightTVar
    unspent     <- if chainHeight > appHeight 
                    then atomically (writeTVar heightTVar chainHeight) >> getUnspentOuts 
                    else pure []
    -- _ <- liftIO $ print appHeight
    -- _ <- liftIO $ print unspent
    let ammOuts = fmap getPoolOperation unspent & catMaybes
        proxyOuts = fmap getPool unspent & catMaybes
    -- _ <- liftIO $ print ammOuts
    -- _ <- liftIO $ print proxyOuts
    _ <- sendProxy proxyOuts
    _ <- sendAmm ammOuts
    pure ()
    