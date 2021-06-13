module Dex.TxOutsProcessor 
    (run) where

import Prelude (print)
import Dex.HttpClient (getUnspentOuts, getCurrentHeight)
import Dex.Models.AppSettings (AppSettings, HasAppSettings(..))
import RIO
import qualified Streamly.Prelude as S

-- use more convenient way to unlift RIO to IO
run :: RIO AppSettings ()
run = do
    heightTvar <- newTVarIO 0
    settings <- view appSettingsL
    liftIO $ 
        S.repeatM (threadDelay 1000000) >> (runRIO settings $ process heightTvar)
            & S.drain

process :: TVar Int -> RIO AppSettings ()
process heightTVar = do
    chainHeight <- getCurrentHeight
    appHeight   <- readTVarIO heightTVar
    unspent     <- if chainHeight > appHeight 
                    then (atomically $ writeTVar heightTVar chainHeight) >> getUnspentOuts 
                    else pure []
    _ <- liftIO $ print appHeight
    _ <- liftIO $ print unspent
    pure ()
    