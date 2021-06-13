module Main where

import Dex.TxOutsProcessor ( run )
import Dex.Models.AppSettings 
    ( HttpSettings(..)
    , BlockRequestSettings(..)
    , AppSettings(..)
    )
import RIO ( runRIO )

main :: IO ()
main = do
    appSettings <- readSettings
    runRIO appSettings $ do run

readSettings :: IO AppSettings
readSettings = do
    let httpSs = HttpSettings "0.0.0.0" 8081 
        blockRequestS = BlockRequestSettings 0
    pure $ AppSettings httpSs blockRequestS
