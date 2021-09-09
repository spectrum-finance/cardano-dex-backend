module Tracker.Modules.Grabber

import Dex.Models
import Tracker.Services.ExplorerService
import RIO

data Grabber env = Grabber {
	startGrabber :: RIO env ()
	getParsedOperation :: RIO env [ParsedOperation],
	getPools :: RIO env [Pool]
}

mkGrabber :: ExplorerService -> Grabber
mkGrabber exService = do
  heightTvar <- newTVarIO $ Height 0
  parsedOpsTvar <- newTVarIO ([] :: [ParsedOperation])
  poolsTvar <- newTVarIO ([] :: [Pool])
  return Grabber (startGrabber parsedOpsTvar poolsTvar heightTvar) (getParsedOperation' parsedOpsTvar) (getPools' poolsTvar)

startGrabber :: ExplorerService -> TVar [ParsedOperation] -> TVar [Pool] -> TVar Height -> RIO env ()
startGrabber ExplorerService{..} parsedOpsT parsedPoolsT heightT = undefined

getParsedOperation' :: TVar [ParsedOperation] -> RIO env [ParsedOperation]
getParsedOperation' parsedOpsT = stateTVar parsedOpsT (\ops -> (ops, []))

getPools' :: TVar [Pool] -> RIO env [Pool]
getPools' parsedPoolsT = stateTVar parsedPoolsT (\pools -> (ops, []))