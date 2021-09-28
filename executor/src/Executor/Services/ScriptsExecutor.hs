{-# LANGUAGE OverloadedStrings #-}

module Executor.Services.ScriptsExecutor where

import Plutus.V1.Ledger.Tx
import Turtle

data ScriptsExecutor {
	sendTx :: Tx -> IO ()
}

mkScriptExecutor :: IO ScriptsExecutor
mkScriptExecutor = pure $ ScriptsExecutor sendTx'

sendTx' :: Tx -> IO ()
sendTx' Tx{..} = do
 toOutput <- undefined
 undefined