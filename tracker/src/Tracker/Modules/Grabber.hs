module Tracker.Modules.Grabber(
	Grabber(..),
	mkGrabber
) where

import Dex.Models
import Tracker.Services.ExplorerService
import Control.Concurrent.STM.TVar (stateTVar)
import Tracker.Models.AppSettings (ExplorerSettings(..), HasExplorerSettings(explorerSettingsL))
import Tracker.Models.ExplorerModels
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
updateOpsAndPools explorer savedHeight prevHeight = do

saveOutputsFromBlock :: ExplorerService -> Height -> IO ()
saveOutputsFromBlock ExplorerService{..} heightToGrab =


getParsedOperation' :: TVar [ParsedOperation] -> IO [ParsedOperation]
getParsedOperation' parsedOpsT =
  atomically (stateTVar parsedOpsT (\prevValue -> (prevValue, [])))

getPools' :: TVar [Pool] -> IO [Pool]
getPools' parsedPoolsT =
  atomically (stateTVar parsedPoolsT (\prevValue -> (prevValue, [])))