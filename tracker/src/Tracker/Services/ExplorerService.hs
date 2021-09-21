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
	getOutputsInBlock :: Id -> IO [ApiFullTxOut]
}

mkExplorerService :: ExplorerSettings -> IO ExplorerService
mkExplorerService exSettings = pure $ ExplorerService (getBestHeight' exSettings) (getOutputsInBlock' exSettings)

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

getTxInBlock' :: ExplorerSettings -> Id -> IO [Transaction]
getTxInBlock' explorerSettings blockId = do
    let request
            = setRequestPath "/blocks/transaction"
            $ setRequestHost "0.0.0.0:9010"
            $ defaultRequest
    response <- liftIO $ httpJSON request
    liftIO $ putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    liftIO $ print $ getResponseHeader "Content-Type" response
    pure (getResponseBody response :: [Transaction])

getOutputsInBlock' :: ExplorerSettings -> Id -> IO [ApiFullTxOut]
getOutputsInBlock' explorerSettings blockId = do
    let request
            = setRequestPath "/blocks/getOutputs"
            $ setRequestHost "0.0.0.0:9010"
            $ defaultRequest
    response <- liftIO $ httpJSON request
    liftIO $ putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    liftIO $ print $ getResponseHeader "Content-Type" response
    pure (getResponseBody response :: [ApiFullTxOut])
