module Main where

import Dex.BlocksProcessor
import Dex.Models.Rosetta.Response.BlockResponse (BlockResponse)
import RIO
import RIO.Text as T
import RIO.Map as Map
import RIO.Map (Map)

main :: IO ()
main = do
    ref <- newIORef $ (Map.fromList [] :: Map Text BlockResponse)
    run 0 ref
