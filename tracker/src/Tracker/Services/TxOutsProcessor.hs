module Tracker.Services.TxOutsProcessor 
    ( TxOutsProcessor(..)
    , mkTxOutsProcessor
    ) where

import Tracker.Services.HttpReqService
import Tracker.Models.AppSettings (AppSettings, HasAppSettings(..))
import RIO
import qualified Streamly.Prelude as S
import Tracker.Services.KafkaService 
import Dex.Processor

data TxOutsProcessor = TxOutsProcessor
    { run :: RIO AppSettings ()
    }

mkTxOutsProcessor :: ProcessorService -> KafkaService AppSettings -> HttpReqService AppSettings -> TxOutsProcessor
mkTxOutsProcessor p k c = TxOutsProcessor $ run' p k c

-- use more convenient way to unlift RIO to IO
run' :: ProcessorService -> KafkaService AppSettings -> HttpReqService AppSettings -> RIO AppSettings ()
run' p k c = do
    heightTvar <- newTVarIO 0
    settings <- view appSettingsL
    liftIO $ 
        S.repeatM (threadDelay 1000000) >> runRIO settings (process p k c heightTvar)
            & S.drain

process :: ProcessorService -> KafkaService AppSettings -> HttpReqService AppSettings -> TVar Int -> RIO AppSettings ()
process ProcessorService{..} KafkaService{..} HttpReqService{..} heightTVar = do
    chainHeight <- getCurrentHeight
    appHeight   <- readTVarIO heightTVar
    unspent     <- if chainHeight > appHeight 
                    then atomically (writeTVar heightTVar chainHeight) >> getUnspentOuts 
                    else pure []
    let ammOuts = fmap getPoolOperation unspent & catMaybes
        proxyOuts = fmap getPool unspent & catMaybes
    _ <- unlessM (pure $ null proxyOuts) (sendProxy proxyOuts)
    _ <- unlessM (pure $ null ammOuts) (sendAmm ammOuts)
    pure ()
    