module Dex.BlocksProcessor 
    (run) where

import Prelude (print)
import Dex.GetBlock
import Dex.Models.Rosetta.Response.BlockResponse (BlockResponse)
import RIO
import RIO.Map as Map
import RIO.Map (Map)

-- add fork checker (sync with current processor)
-- add cache (redis) in case to store last max height and current unused outputs
-- check if txn's outputs contain datum hash with proxy contract
run :: Int -> IORef (Map Text BlockResponse) -> IO ()
run ref = do
    r <- getBlock height
    _ <- print r
    run (height + 1) ref
    