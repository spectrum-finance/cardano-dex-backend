module Dex.TxOutsProcessor 
    (run) where

import Prelude (print)
import Dex.HttpClient (getUnspendOutsStream)
import Dex.Models.AppSettings (AppSettings)
import RIO

run :: Int -> IORef (Map Text ()) -> RIO AppSettings ()
run height ref = do
    r <- getUnspendOutsStream
    liftIO $ print r
    
    