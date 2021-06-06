module Main where

import Dex.BlocksProcessor
import Dex.Models.Rosetta.Response.BlockResponse (BlockResponse)
import Dex.Models.AppSettings (HttpSettings, HttpSettings(..))
import RIO
import RIO.Text as T
import RIO.Map as Map
import RIO.Map (Map)

main :: IO ()
main = do
    let settings = HttpSettings (T.pack "0.0.0.0") 8080 (T.pack "testnet") (T.pack "cardano")
    ref <- newIORef $ (Map.fromList [] :: Map Text BlockResponse)
    runRIO settings $ do
        run 0 ref