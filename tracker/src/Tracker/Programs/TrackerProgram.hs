module Tracker.Programs.TrackerProgram where

import qualified Streamly.Prelude as S
import RIO 
import Tracker.Services.ExplorerService
import Tracker.Services.KafkaService
import Tracker.Utils
import ErgoDex.Class
import ErgoDex.Amm.Orders
import ErgoDex.Amm.Pool
import ErgoDex.State
import Data.Aeson
import qualified RIO.ByteString.Lazy as BL
import GHC.Natural as Natural
import Core.Streaming
import Tracker.Models.AppSettings 
import Streaming.Producer
import Cardano.Models
import Explorer.Types
import Cardano.Models as Sdk
import Explorer.Models as Explorer

data TrackerProgram f = TrackerProgram 
  { run :: f () 
  }

mkTrackerProgram 
  :: (Monad f) 
  => ExplorerProgrammSettings
  -> ExplorerService f
  -> Producer f String ConfirmedOrderEvent 
  -> Producer f String ConfirmedPoolEvent 
  -> TrackerProgram f
mkTrackerProgram settings explorer orderProd poolProd = 
  TrackerProgram $ run' settings explorer orderProd poolProd

run' 
  :: (Monad f) 
  => ExplorerProgrammSettings
  -> ExplorerService f
  -> Producer f String ConfirmedOrderEvent
  -> Producer f String ConfirmedPoolEvent
  -> f ()
run' ExplorerProgrammSettings{..} explorer orderProd poolProd =
    S.repeatM (threadDelay $ Natural.naturalToInt pollTime) >> process explorer orderProd poolProd --todo threadDelay replace with streamly api
  -- & --todo err handle
  & S.drain

process 
  :: ExplorerService f
  -> Producer f String ConfirmedOrderEvent
  -> Producer f String ConfirmedPoolEvent 
  -> f ()
process ExplorerService{..} orderProd poolProd = do
  fulltxOuts <- getOutputs
  let
    unspent = (map toFullTxOut fulltxOuts) `zip` fulltxOuts

    confirmedOrderEvents = 
         swapEvents ++ depositEvents ++ redeemEvents
      where
        swapEvents    = mkSwapEvents (parseAmm unspent :: [(Confirmed Swap, Gix)]) 
        depositEvents = mkDepositEvents (parseAmm unspent :: [(Confirmed Deposit, Gix)])
        redeemEvents  = mkRedeemEvents (parseAmm unspent :: [(Confirmed Redeem, Gix)])
  
    -- confirmedPoolEvents =
    --     pairToPoolEvent confirmedPools
    --   where
    --     confirmedPools = parseAmm unspent :: [(Confirmed Pool, Integer)]
  
  -- _ <- produce orderProd confirmedOrderEvents
  -- _ <- produce poolProd confirmedPoolEvents
  pure ()
  -- print $ "TrackerProgramm::newAmmOutputsLength=" ++ show (length res1)
  -- print $ "TrackerProgramm::newProxyOutputs=" ++ show (length msgs)
  -- unless (null res1) (sendAmm res1 >> print "TrackerProgramm::AmmSuccessfullySentIntoKafka")
  -- unless (null msgs) (sendProxy msgs >> print "TrackerProgramm::ProxySuccessfullySentIntoKafka")

mkSwapEvents
  :: [(Confirmed Swap, Gix)]
  -> [ConfirmedOrderEvent]
mkSwapEvents swaps =
  fmap (\((Confirmed fullTxOut s@Swap{..}), gix) -> ConfirmedOrderEvent (AnyOrder swapPoolId (SwapAction s)) fullTxOut gix) swaps

mkRedeemEvents
  :: [(Confirmed Redeem, Gix)]
  -> [ConfirmedOrderEvent]
mkRedeemEvents redeems =
  fmap (\((Confirmed fullTxOut s@Redeem{..}), gix) -> ConfirmedOrderEvent (AnyOrder redeemPoolId (RedeemAction s)) fullTxOut gix) redeems

mkDepositEvents
  :: [(Confirmed Deposit, Gix)]
  -> [ConfirmedOrderEvent]
mkDepositEvents deposits =
  fmap (\((Confirmed fullTxOut s@Deposit{..}), gix) -> ConfirmedOrderEvent (AnyOrder depositPoolId (DepositAction s)) fullTxOut gix) deposits

parseAmm
  :: (FromLedger amm)
  => [(Sdk.FullTxOut, Explorer.FullTxOut)]
  -> [(Confirmed amm, Gix)]
parseAmm unspent = 
  mapMaybe (\(out, Explorer.FullTxOut{..}) -> (\r -> (r, globalIndex)) `fmap` (parseFromLedger out)) unspent

pairToPoolEvent :: [(Confirmed Pool, Gix)] -> [ConfirmedPoolEvent]
pairToPoolEvent pairs = 
  fmap (\((Confirmed out pool), gix) -> ConfirmedPoolEvent pool out gix) pairs