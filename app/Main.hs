module Main where

import Dex.TxOutsProcessor
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
    ref <- newIORef $ (Map.fromList [] :: Map Text ())
    runRIO appSettings $ do
        run 0 ref

readSettings :: IO AppSettings
readSettings = do
    let httpSs = HttpSettings "0.0.0.0" "8081" 
        blockRequestS = BlockRequestSettings 10
    pure $ AppSettings httpSs blockRequestS
