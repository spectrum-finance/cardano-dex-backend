module Tracker.Programs.TrackerProgram where

import Streaming.Events
import Streaming.Producer
import Streaming.Types
import Debug.Trace

import Tracker.Services.TrackerService
import Tracker.Models.AppConfig

import ErgoDex.Class
import System.Logging.Hlog
import ErgoDex.Amm.Orders
import ErgoDex.Amm.Pool
import Debug.Trace as Trace
import ErgoDex.State

import Explorer.Models as Explorer
import Explorer.Types

import qualified Streamly.Prelude as S
import           RIO
import           GHC.Natural as Natural
import           Control.Monad.Catch
import Explorer.Class (ToCardanoTx(toCardanoTx))

data TrackerProgram f = TrackerProgram
  { run :: f ()
  }

mkTrackerProgram
  :: (Monad i, S.MonadAsync f, MonadCatch f)
  => TrackerProgrammConfig
  -> MakeLogging i f
  -> TrackerService f
  -> Producer f PoolId ConfirmedOrderEvent
  -> Producer f PoolId ConfirmedPoolEvent
  -> i (TrackerProgram f)
mkTrackerProgram settings MakeLogging{..} explorer orderProd poolProd = do
  logger <- forComponent "trackerProgram"
  pure $ TrackerProgram $ run' settings logger explorer orderProd poolProd

run'
  :: (S.MonadAsync f, MonadCatch f)
  => TrackerProgrammConfig
  -> Logging f
  -> TrackerService f
  -> Producer f PoolId ConfirmedOrderEvent
  -> Producer f PoolId ConfirmedPoolEvent
  -> f ()
run' TrackerProgrammConfig{..} logging@Logging{..} explorer orderProd poolProd =
    S.repeatM (process explorer logging orderProd poolProd)
  & S.delay (fromIntegral $ Natural.naturalToInt pollTime)
  & S.handle (\(a :: SomeException) -> (lift . errorM $ ("tracker stream error: " ++ (show a)))) -- log.info here
  & S.drain

process
  :: (Monad f)
  => TrackerService f
  -> Logging f
  -> Producer f PoolId ConfirmedOrderEvent
  -> Producer f PoolId ConfirmedPoolEvent
  -> f ()
process TrackerService{..} Logging{..} orderProd poolProd = do
  utxos <- getOutputs
  let
    confirmedOrderEvents =
        swapEvents ++ depositEvents ++ redeemEvents
      where
        swapEvents    = mkSwapEvents $ parseOnChainEntity utxos
        depositEvents = mkDepositEvents $ parseOnChainEntity utxos
        redeemEvents  = mkRedeemEvents $ parseOnChainEntity utxos

    confirmedPoolEvents = mkPoolEvents $ parseOnChainEntity utxos
  _ <- infoM ("confirmedPoolEvents in batch: "  ++ (show (length confirmedPoolEvents)))
  _ <- infoM ("confirmedPoolEvents in batch: "  ++ (show (confirmedPoolEvents)))
  _ <- infoM ("confirmedOrderEvents in batch: " ++ (show (length confirmedOrderEvents)))
  unless (null confirmedOrderEvents) (produce orderProd (S.fromList confirmedOrderEvents))
  unless (null confirmedPoolEvents) (produce poolProd (S.fromList confirmedPoolEvents))

mkSwapEvents
  :: [(OnChain Swap, Gix)]
  -> [(PoolId, ConfirmedOrderEvent)]
mkSwapEvents =
  fmap (\(OnChain fullTxOut s@Swap{..}, gix) -> (swapPoolId, ConfirmedOrderEvent (AnyOrder swapPoolId (SwapAction s)) fullTxOut gix))

mkRedeemEvents
  :: [(OnChain Redeem, Gix)]
  -> [(PoolId, ConfirmedOrderEvent)]
mkRedeemEvents =
  fmap (\(OnChain fullTxOut s@Redeem{..}, gix) -> (redeemPoolId, ConfirmedOrderEvent (AnyOrder redeemPoolId (RedeemAction s)) fullTxOut gix))

mkDepositEvents
  :: [(OnChain Deposit, Gix)]
  -> [(PoolId, ConfirmedOrderEvent)]
mkDepositEvents =
  fmap (\(OnChain fullTxOut s@Deposit{..}, gix) -> (depositPoolId, ConfirmedOrderEvent (AnyOrder depositPoolId (DepositAction s)) fullTxOut gix))

mkPoolEvents
  :: [(OnChain Pool, Gix)]
  -> [(PoolId, ConfirmedPoolEvent)]
mkPoolEvents =
  fmap (\(OnChain fullTxOut pool@Pool{..}, gix) -> (poolId, ConfirmedPoolEvent pool fullTxOut gix))

parseOnChainEntity
  :: FromLedger amm
  => [Explorer.FullTxOut]
  -> [(OnChain amm, Gix)]
parseOnChainEntity =
  mapMaybe (\out@Explorer.FullTxOut{..} -> (, globalIndex) <$> parseFromLedger (toCardanoTx out))
