module Dex.TxOutsProcessor 
    (run) where

import Prelude (print)
import Dex.HttpClient (getUnspentOuts, getCurrentHeight)
import Dex.Models.AppSettings (AppSettings, HasAppSettings(..))
import RIO
import qualified Streamly.Prelude as S
import Streamly
import RIO.Prelude as R ((>>))
import RIO.List as List
import Dex.Models.ApiTxOut ( ApiTxOut )
import Control.Monad.Trans.Control

-- use more convenient way to unlift RIO to IO
run :: RIO AppSettings ()
run = do
    heightTvar <- newTVarIO 0
    settings <- view appSettingsL
    liftIO $ 
        S.repeatM (runRIO settings $ process heightTvar)
            & constRate 1
            & S.drain

process :: TVar Int -> RIO AppSettings ()
process heightTVar = do
    chainHeight <- getCurrentHeight
    appHeight   <- readTVarIO heightTVar
    unspent     <- if chainHeight > appHeight 
                    then (atomically $ writeTVar heightTVar chainHeight) >> getUnspentOuts 
                    else pure []
    _ <- liftIO $ print appHeight
    pure ()
    