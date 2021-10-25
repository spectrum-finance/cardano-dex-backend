module Tracker.Programs.TrackerProgram where

import qualified Streamly.Prelude as S
import RIO 
import Tracker.Services.ExplorerService
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
  :: (Monad f, S.MonadAsync f) 
  => ExplorerProgrammSettings
  -> ExplorerService f
  -> Producer f PoolId ConfirmedOrderEvent 
  -> Producer f PoolId ConfirmedPoolEvent 
  -> TrackerProgram f
mkTrackerProgram settings explorer orderProd poolProd = 
  TrackerProgram $ run' settings explorer orderProd poolProd

run' 
  :: (Monad f, S.MonadAsync f) 
  => ExplorerProgrammSettings
  -> ExplorerService f
  -> Producer f PoolId ConfirmedOrderEvent
  -> Producer f PoolId ConfirmedPoolEvent
  -> f ()
run' ExplorerProgrammSettings{..} explorer orderProd poolProd =
    S.repeatM ((threadDelay $ Natural.naturalToInt pollTime) >> process explorer orderProd poolProd) --todo threadDelay replace with streamly api
  & S.drain 
  -- & --todo err handle
  
process 
  :: (Monad f)
  => ExplorerService f
  -> Producer f PoolId ConfirmedOrderEvent
  -> Producer f PoolId ConfirmedPoolEvent 
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
  
    confirmedPoolEvents =
        mkPoolEvents confirmedPools
      where
        confirmedPools = parseAmm unspent :: [(Confirmed Pool, Gix)]

  unless (null confirmedOrderEvents) (produce orderProd (S.fromList confirmedOrderEvents))
  unless (null confirmedOrderEvents) (produce poolProd (S.fromList confirmedPoolEvents))

mkSwapEvents
  :: [(Confirmed Swap, Gix)]
  -> [(PoolId, ConfirmedOrderEvent)]
mkSwapEvents swaps =
  fmap (\((Confirmed fullTxOut s@Swap{..}), gix) -> (swapPoolId, ConfirmedOrderEvent (AnyOrder swapPoolId (SwapAction s)) fullTxOut gix)) swaps

mkRedeemEvents
  :: [(Confirmed Redeem, Gix)]
  -> [(PoolId, ConfirmedOrderEvent)]
mkRedeemEvents redeems =
  fmap (\((Confirmed fullTxOut s@Redeem{..}), gix) -> (redeemPoolId, ConfirmedOrderEvent (AnyOrder redeemPoolId (RedeemAction s)) fullTxOut gix)) redeems

mkDepositEvents
  :: [(Confirmed Deposit, Gix)]
  -> [(PoolId, ConfirmedOrderEvent)]
mkDepositEvents deposits =
  fmap (\((Confirmed fullTxOut s@Deposit{..}), gix) -> (depositPoolId, ConfirmedOrderEvent (AnyOrder depositPoolId (DepositAction s)) fullTxOut gix)) deposits

mkPoolEvents
  :: [(Confirmed Pool, Gix)]
  -> [(PoolId, ConfirmedPoolEvent)]
mkPoolEvents pools =
  fmap (\((Confirmed fullTxOut pool@Pool{..}), gix) -> (poolId, ConfirmedPoolEvent pool fullTxOut gix)) pools

parseAmm
  :: (FromLedger amm)
  => [(Sdk.FullTxOut, Explorer.FullTxOut)]
  -> [(Confirmed amm, Gix)]
parseAmm unspent = 
  mapMaybe (\(out, Explorer.FullTxOut{..}) -> (\r -> (r, globalIndex)) `fmap` (parseFromLedger out)) unspent