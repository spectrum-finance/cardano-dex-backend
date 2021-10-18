module Tracker.Programs.TrackerProgram where

import System.IO
import qualified Streamly.Prelude as S
import RIO 
import Tracker.Services.ExplorerService
import Tracker.Services.KafkaService
import Tracker.Utils
import ErgoDex.Class
import ErgoDex.Amm.Orders
import ErgoDex.Amm.Pool
import ErgoDex.State

data TrackerProgram = TrackerProgram 
  { run :: IO () 
  }

mkTrackerProgram :: ExplorerService -> KafkaService -> IO TrackerProgram
mkTrackerProgram e k = pure $ TrackerProgram $ publishStream e k

publishStream :: ExplorerService -> KafkaService -> IO ()
publishStream e k =
  S.repeatM (threadDelay 1000000) >> liftIO (process e k) & S.drain

process :: ExplorerService -> KafkaService -> IO ()
process ExplorerService{..} KafkaService{..} = do
  fulltxOuts <- getOutputs
  unspent <- mapM toFullTxOut fulltxOuts
  let (ammOuts :: [Confirmed Pool]) = mapMaybe parseFromLedger unspent
      (sw :: [Confirmed Swap]) = mapMaybe parseFromLedger unspent
      (de :: [Confirmed Deposit]) = mapMaybe parseFromLedger unspent
      (re :: [Confirmed Redeem]) = mapMaybe parseFromLedger unspent
      res = (map (\x@(Confirmed _ r@Swap{..}) -> AnyOrder swapPoolId (SwapAction r)) sw) ++ (map (\x@(Confirmed _ r@Deposit{..}) -> AnyOrder depositPoolId (DepositAction r)) de) ++ (map (\x@(Confirmed _ r@Redeem{..}) -> AnyOrder redeemPoolId (RedeemAction r)) re)
      res1 = map (\x@(Confirmed _ r@Pool{..}) -> r) ammOuts
  print $ "TrackerProgramm::newAmmOutputsLength=" ++ show (length res1)
  print $ "TrackerProgramm::newProxyOutputs=" ++ show (length res)
  unless (null res1) (sendAmm res1 >> print "TrackerProgramm::AmmSuccessfullySentIntoKafka")
  unless (null res) (sendProxy res >> print "TrackerProgramm::ProxySuccessfullySentIntoKafka")