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
import Tracker.Models.KafkaModel
import Data.Aeson
import qualified RIO.ByteString.Lazy as BL

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
      kafkaMsg1 = map (\(Confirmed out order@Swap{..}) -> KafkaMsg out swapPoolId (BL.toStrict $ encode order)) sw
      kafkaMsg2 = map (\(Confirmed out order@Deposit{..}) -> KafkaMsg out depositPoolId (BL.toStrict $ encode order)) de
      kafkaMsg3 = map (\(Confirmed out order@Redeem{..}) -> KafkaMsg out redeemPoolId (BL.toStrict $ encode order)) re
      msgs = kafkaMsg1 ++ kafkaMsg2 ++ kafkaMsg3
      res1 = map (\(Confirmed out pool@Pool{..}) -> KafkaMsg out poolId (BL.toStrict $ encode pool)) ammOuts
  print $ "TrackerProgramm::newAmmOutputsLength=" ++ show (length res1)
  print $ "TrackerProgramm::newProxyOutputs=" ++ show (length msgs)
  unless (null res1) (sendAmm res1 >> print "TrackerProgramm::AmmSuccessfullySentIntoKafka")
  unless (null msgs) (sendProxy msgs >> print "TrackerProgramm::ProxySuccessfullySentIntoKafka")