module Tracker.Programs.TrackerProgram where

import System.IO
import qualified Streamly.Prelude as S
import RIO 
import Tracker.Services.ExplorerService
import Tracker.Services.KafkaService
import Dex.Processor
import Tracker.Utils

data TrackerProgram = TrackerProgram 
  { run :: IO () 
  }

mkTrackerProgram :: ProcessorService -> ExplorerService -> KafkaService -> IO TrackerProgram
mkTrackerProgram p e k = pure $ TrackerProgram $ publishStream p e k

publishStream :: ProcessorService -> ExplorerService -> KafkaService -> IO ()
publishStream p e k =
  S.repeatM (threadDelay 1000000) >> liftIO (process p e k) & S.drain

process :: ProcessorService -> ExplorerService -> KafkaService -> IO ()
process ProcessorService{..} ExplorerService{..} KafkaService{..} = do
  fulltxOuts <- getOutputs
  let unspent = fmap toFullTxOut fulltxOuts
      ammOuts = mapMaybe getPool unspent
      proxyOuts = mapMaybe getPoolOperation unspent
  print $ "TrackerProgramm::newAmmOutputsLength=" ++ show (length ammOuts)
  print $ "TrackerProgramm::newProxyOutputs=" ++ show (length proxyOuts)
  unless (null ammOuts) (sendAmm ammOuts)
  unless (null proxyOuts) (sendProxy proxyOuts)
  print "TrackerProgramm::Kafka messages produces successfully"