module Main where

import Dex.BlocksProcessor
import Dex.Models.Rosetta.Response.BlockResponse (BlockResponse)
import Dex.Models.AppSettings 
    ( HttpSettings(..)
    , BlockRequestSettings(..)
    , AppSettings(..)
    )
import RIO
import RIO.Text as T
import RIO.Map as Map
import RIO.Map (Map)

main :: IO ()
main = do
    appSettings <- readSettings
    ref <- newIORef $ (Map.fromList [] :: Map Text BlockResponse)
    runRIO appSettings $ do
        run 0 ref

readSettings :: IO AppSettings
readSettings = do
    let httpSs = HttpSettings (T.pack "0.0.0.0") 8080 (T.pack "testnet") (T.pack "cardano")
        blockRequestS = BlockRequestSettings 10
    pure $ AppSettings httpSs blockRequestS
