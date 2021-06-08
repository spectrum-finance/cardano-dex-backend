module Dex.TxOutsProcessor 
    (run) where

import Prelude (print)
import Dex.HttpClient (getUnspendOuts)
import Dex.Models.AppSettings (AppSettings)
import RIO
import RIO.Map as Map
import RIO.Map (Map)

run :: Int -> IORef (Map Text ()) -> RIO AppSettings ()
run height ref = do
    r <- getUnspendOuts
    liftIO $ print r
    
    