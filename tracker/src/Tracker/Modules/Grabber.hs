module Tracker.Modules.Grabber(
	Grabber(..),
	mkGrabber
) where

import Dex.Models
import Data.Maybe
import Tracker.Services.ExplorerService
import Tracker.Utils
import Dex.Processor
import Control.Concurrent.STM.TVar (stateTVar)
import Tracker.Models.AppSettings (ExplorerSettings(..), HasExplorerSettings(explorerSettingsL))
import Tracker.Models.ExplorerModels
    ( ApiFullTxOut, Transaction(txHash), Height(Height) )
import Data.List
import RIO
import Control.Loop
import qualified Streamly.Prelude as S

data Grabber = Grabber {
	startGrabber :: IO (),
	getParsedOperation :: IO [ParsedOperation],
	getPools :: IO [Pool]
}

mkGrabber :: ExplorerService -> IO Grabber
mkGrabber exService = do
  heightTvar <- newTVarIO $ Height 0
  parsedOpsTvar <- newTVarIO ([] :: [ParsedOperation])
  poolsTvar <- newTVarIO ([] :: [Pool])
  return $ Grabber (startGrabber' exService parsedOpsTvar poolsTvar heightTvar) (getParsedOperation' parsedOpsTvar) (getPools' poolsTvar)

startGrabber' :: ExplorerService -> TVar [ParsedOperation] -> TVar [Pool] -> TVar Height -> IO ()
startGrabber' explorer parsedOpsT parsedPoolsT heightT =
   liftIO $ S.repeatM (threadDelay 1000000) >> (liftIO $ grab explorer parsedOpsT parsedPoolsT heightT) & S.drain

grab :: ExplorerService -> TVar [ParsedOperation] -> TVar [Pool] -> TVar Height -> IO ()
grab explorer parsedOpsT parsedPoolsT heightT = do
  savedHeight <- readTVarIO heightT
  currentBlockchainHeight <- getBestHeight explorer
  _ <- if (savedHeight < currentBlockchainHeight)
        then updateOpsAndPools explorer savedHeight currentBlockchainHeight
        else pure ()
  pure ()

updateOpsAndPools :: ExplorerService -> Height -> Height -> IO ()
updateOpsAndPools explorer savedHeight prevHeight = undefined
 
saveOutputsFromBlock :: ExplorerService -> Height -> IO ()
saveOutputsFromBlock explorer heightToGrab = do
  txsOnHeight <- (getTxsInBlock explorer) heightToGrab
  fulltxOuts <- forLoopState 0 (<= length txsOnHeight) (+1) ([]) (getOutputsFromTx explorer txsOnHeight)
  let processorService = mkProcessorService
      unspent = fmap toFullTxOut fulltxOuts
      ammOuts = fmap (getPool processorService) unspent & catMaybes
      proxyOuts = fmap (getPoolOperation processorService) unspent & catMaybes
  pure ()

getOutputsFromTx :: ExplorerService -> [Transaction] -> [ApiFullTxOut] -> Int -> IO [ApiFullTxOut]
getOutputsFromTx ExplorerService{..} txsList acc txId = do
  let tx = txsList!!txId
  txOutputs <- getOutputsInTx (txHash tx) txId
  pure $ acc ++ txOutputs

getParsedOperation' :: TVar [ParsedOperation] -> IO [ParsedOperation]
getParsedOperation' parsedOpsT =
  atomically (stateTVar parsedOpsT (\prevValue -> (prevValue, [])))

getPools' :: TVar [Pool] -> IO [Pool]
getPools' parsedPoolsT =
  atomically (stateTVar parsedPoolsT (\prevValue -> (prevValue, [])))