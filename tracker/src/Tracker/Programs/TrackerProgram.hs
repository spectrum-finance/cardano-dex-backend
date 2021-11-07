module Tracker.Programs.TrackerProgram where

import Streaming.Events
import Streaming.Producer
import Streaming.Types

import Tracker.Services.TrackerService
import Tracker.Models.AppConfig
import Tracker.Utils

import ErgoDex.Class
import ErgoDex.Amm.Orders
import ErgoDex.Amm.Pool
import ErgoDex.State
import Cardano.Models     as Sdk
import Explorer.Models    as Explorer
import Explorer.Types

import qualified Streamly.Prelude as S
import           RIO 
import           GHC.Natural as Natural
import           Control.Monad.Catch

data TrackerProgram f = TrackerProgram 
  { run :: f () 
  }

mkTrackerProgram 
  :: (S.MonadAsync f, MonadCatch f) 
  => TrackerProgrammConfig
  -> TrackerService f
  -> Producer f PoolId ConfirmedOrderEvent 
  -> Producer f PoolId ConfirmedPoolEvent 
  -> TrackerProgram f
mkTrackerProgram settings explorer orderProd poolProd = 
  TrackerProgram $ run' settings explorer orderProd poolProd

run' 
  :: (S.MonadAsync f, MonadCatch f) 
  => TrackerProgrammConfig
  -> TrackerService f
  -> Producer f PoolId ConfirmedOrderEvent
  -> Producer f PoolId ConfirmedPoolEvent
  -> f ()
run' TrackerProgrammConfig{..} explorer orderProd poolProd =
    S.repeatM (process explorer orderProd poolProd)
  & S.delay (fromIntegral $ Natural.naturalToInt pollTime)
  & S.handle (\ProducerExecption -> S.fromPure ()) -- log.info here
  & S.drain 
  
process 
  :: (Monad f)
  => TrackerService f
  -> Producer f PoolId ConfirmedOrderEvent
  -> Producer f PoolId ConfirmedPoolEvent 
  -> f ()
process TrackerService{..} orderProd poolProd = do
  fulltxOuts <- getOutputs
  let
    unspent = map toFullTxOut fulltxOuts `zip` fulltxOuts

    confirmedOrderEvents = 
         swapEvents ++ depositEvents ++ redeemEvents
      where
        swapEvents    = mkSwapEvents (parseAmm unspent :: [(Confirmed Swap, Gix)]) 
        depositEvents = mkDepositEvents (parseAmm unspent :: [(Confirmed Deposit, Gix)])
        redeemEvents  = mkRedeemEvents (parseAmm unspent :: [(Confirmed Redeem, Gix)])
  
    confirmedPoolEvents =
        mkPoolEvents confirmedPools
      where
        confirmedPools = parseAmm unspent :: [(Confirmed Pool, Gix)]

  unless (null confirmedOrderEvents) (produce orderProd (S.fromList confirmedOrderEvents))
  unless (null confirmedOrderEvents) (produce poolProd (S.fromList confirmedPoolEvents))

mkSwapEvents
  :: [(Confirmed Swap, Gix)]
  -> [(PoolId, ConfirmedOrderEvent)]
mkSwapEvents =
  fmap (\(Confirmed fullTxOut s@Swap{..}, gix) -> (swapPoolId, ConfirmedOrderEvent (AnyOrder swapPoolId (SwapAction s)) fullTxOut gix))

mkRedeemEvents
  :: [(Confirmed Redeem, Gix)]
  -> [(PoolId, ConfirmedOrderEvent)]
mkRedeemEvents =
  fmap (\(Confirmed fullTxOut s@Redeem{..}, gix) -> (redeemPoolId, ConfirmedOrderEvent (AnyOrder redeemPoolId (RedeemAction s)) fullTxOut gix))

mkDepositEvents
  :: [(Confirmed Deposit, Gix)]
  -> [(PoolId, ConfirmedOrderEvent)]
mkDepositEvents =
  fmap (\(Confirmed fullTxOut s@Deposit{..}, gix) -> (depositPoolId, ConfirmedOrderEvent (AnyOrder depositPoolId (DepositAction s)) fullTxOut gix))

mkPoolEvents
  :: [(Confirmed Pool, Gix)]
  -> [(PoolId, ConfirmedPoolEvent)]
mkPoolEvents =
  fmap (\(Confirmed fullTxOut pool@Pool{..}, gix) -> (poolId, ConfirmedPoolEvent pool fullTxOut gix))

parseAmm
  :: (FromLedger amm)
  => [(Sdk.FullTxOut, Explorer.FullTxOut)]
  -> [(Confirmed amm, Gix)]
parseAmm = 
  mapMaybe (\(out, Explorer.FullTxOut{..}) -> (, globalIndex) <$> parseFromLedger out)