{-# LANGUAGE OverloadedStrings #-}

module Tracker.Services.ExplorerService (
	ExplorerService(..),
	mkExplorerService
) where

import RIO
import Tracker.Models.ExplorerModels
import qualified Data.Yaml             as Yaml
import Prelude
import Tracker.Models.AppSettings (ExplorerSettings(..), HasExplorerSettings(explorerSettingsL), AppSettings(..))
import Network.HTTP.Simple

data ExplorerService = ExplorerService {
	getBestHeight :: IO Height,
	getOutputsInTx :: String -> Int -> IO [ApiFullTxOut]
	getTxsInBlock :: Height -> IO [Transaction]
}

mkExplorerService :: ExplorerSettings -> IO ExplorerService
mkExplorerService exSettings = pure $ ExplorerService (getBestHeight' exSettings) (getOutputsInTx' exSettings) (getTxsInBlock' exSettings)

getBestHeight' :: ExplorerSettings -> IO Height
getBestHeight' explorerSettings = do
  let request
          = setRequestPath "/blocks/getBestHeight"
          $ setRequestHost "0.0.0.0:9010"
          $ defaultRequest
  response <- liftIO $ httpJSON request
  liftIO $ putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
  liftIO $ print $ getResponseHeader "Content-Type" response
  pure (getResponseBody response :: Height)

getTxsInBlock' :: ExplorerSettings -> Height -> IO [Transaction]
getTxsInBlock' explorerSettings height = do
    let request
            = setRequestPath "/blocks/transaction"
            $ setRequestHost "0.0.0.0:9010"
            $ defaultRequest
    response <- liftIO $ httpJSON request
    liftIO $ putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    liftIO $ print $ getResponseHeader "Content-Type" response
    pure (getResponseBody response :: [Transaction])

getOutputsInTx' :: ExplorerSettings -> String -> Int -> IO [ApiFullTxOut]
getOutputsInTx' explorerSettings txHash index = do
    let request
            = setRequestPath "/blocks/getOutputs"
            $ setRequestHost "0.0.0.0:9010"
            $ defaultRequest
    response <- liftIO $ httpJSON request
    liftIO $ putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    liftIO $ print $ getResponseHeader "Content-Type" response
    pure (getResponseBody response :: [ApiFullTxOut])
