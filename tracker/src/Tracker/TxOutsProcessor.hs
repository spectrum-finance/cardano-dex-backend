module Tracker.TxOutsProcessor 
    (run) where

import Prelude (print)
import Tracker.HttpClient (getUnspentOuts, getCurrentHeight)
import Tracker.Models.AppSettings (AppSettings, HasAppSettings(..))
import RIO
import qualified Streamly.Prelude as S
import Tracker.KafkaClient
import Dex.Processor

-- use more convenient way to unlift RIO to IO
run :: RIO AppSettings ()
run = do
    heightTvar <- newTVarIO 0
    settings <- view appSettingsL
    liftIO $ 
        S.repeatM (threadDelay 1000000) >> runRIO settings (process heightTvar)
            & S.drain

process :: TVar Int -> RIO AppSettings ()
process heightTVar = do
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
    