module Dex.TxOutsProcessor 
    (run) where

import Prelude (print)
import Dex.HttpClient (getUnspentOuts)
import Dex.Models.AppSettings (AppSettings)
import RIO

run :: Int -> IORef (Map Text ()) -> RIO AppSettings ()
run height ref = do
    r <- getUnspentOuts
    liftIO $ print r
    
    