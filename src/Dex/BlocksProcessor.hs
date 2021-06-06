module Dex.BlocksProcessor 
    (run) where

import Prelude (print)
import Dex.GetBlock
import Dex.Models.Rosetta.Response.BlockResponse (BlockResponse)
import Dex.Models.AppSettings
import RIO
import RIO.Map as Map
import RIO.Map (Map)
import Plutus.Contract.Schema ()

-- add fork checker (sync with current processor)
-- add cache (redis) in case to store last max height and current unused outputs
-- check if txn's outputs contain datum hash with proxy contract
-- check if sent txn is in chain and remove used output
run :: Int -> IORef (Map Text BlockResponse) -> RIO HttpSettings ()
run height ref = do
    r <- getBlock height
    _ <- liftIO $ print r
    run (height + 1) ref
    