module Dex.TxOutsProcessor 
    (run) where

import Prelude (print)
import Dex.HttpClient (getUnspentOuts, getCurrentHeight)
import Dex.Models.AppSettings (AppSettings)
import RIO as R
import qualified Streamly.Prelude as S
import Streamly

run :: Int -> IORef (Map Text ()) -> RIO AppSettings ()
run height ref = do
    r <- getUnspentOuts
    s <- getCurrentHeight
    _ <- liftIO $ print r
    liftIO $ print s

run' :: TVar Int
run' heightTVar =
    S.repeatM getCurrentHeight
        & S.mapM \newHeight ->
            R.>>= 
                (readTVarIO heightTVar) 
                (\height -> 
                    if (newHeight > heigh) do
                        R.>> (writeTVar heightTVar newHeight) getUnspentOuts
                    else R.pure []
                )
        & avgRate 5
    
    