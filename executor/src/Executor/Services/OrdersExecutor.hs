module Executor.Services.OrdersExecutor
  ( OrdersExecutor(..)
  , mkOrdersExecutor
  ) where

import SdkCore.Throw.Combinators
import Core.Extractor.AlonzoTxExtractors
import Core.Extractor.Class
import Executor.Services.PoolsResolver
import Control.Monad.Catch (MonadThrow(..), try)
import Executor.Models.Errors
import Tracker.Services.Logger as Log
import RIO
import qualified Prelude

import ErgoDex.State 
import ErgoDex.Amm.Pool
import ErgoDex.Amm.Orders
import ErgoDex.Amm.PoolActions
import CardanoTx.Models
import SubmitAPI.Service
import Core.Types

throwEither1 :: (MonadThrow f, Exception e, Show r, MonadIO f) => Either e r -> f r
throwEither1 (Left err)    = throwM err
throwEither1 (Right value) = do
  _ <- Log.log (show value)
  pure value

data OrdersExecutor f = OrdersExecutor
  { process :: Confirmed AnyOrder -> f () 
  }

mkOrdersExecutor 
  :: (MonadThrow f, MonadIO f)
  => PoolActions
  -> PoolsResolver f
  -> Transactions f
  -> OrdersExecutor f
mkOrdersExecutor pa pr submitService = OrdersExecutor $ process' pa pr submitService

process' 
  :: (MonadThrow f, MonadIO f)
  => PoolActions 
  -> PoolsResolver f
  -> Transactions f
  -> Confirmed AnyOrder
  -> f ()
process' poolActions PoolsResolver{..} Transactions{..} confirmedOrder@(Confirmed _ (AnyOrder poolId _)) = do
  _ <- Log.log "process1"
  maybePool                      <- resolvePool poolId
  _ <- Log.log ("process2. " ++ (show maybePool))
  pool@(ConfirmedPool confPool)  <- throwMaybe EmptyPoolErr maybePool
  _ <- Log.log "process3"
  let
    maybeTx = runOrder pool confirmedOrder poolActions
  _ <- Log.log "process4"
  (txCandidate, predictedPool) <- throwEither1 maybeTx
  _ <- Log.log ("txCandidate: " ++ (show predictedPool))
  _ <- Log.log "process5"
  finalTx                        <- finalizeTx txCandidate
  _ <- Log.log "process6"
  let
    mPool = extract finalTx :: Maybe (FullTxOut, Pool)
  _ <- Log.log "process7"
  (out, pp)                      <- throwMaybe EmptyPoolErr mPool
  _ <- Log.log "process8"
  let
    predictedPool = PredictedPool $ OnChainIndexedEntity pp out (getGix confPool)
  _ <- Log.log "process9"
  _                              <- sendPredicted predictedPool
  _ <- Log.log "process10"
  submitTx finalTx

runOrder
  :: ConfirmedPool
  -> Confirmed AnyOrder 
  -> PoolActions
  -> Either OrderExecErr (TxCandidate, Predicted Pool)
runOrder (ConfirmedPool (OnChainIndexedEntity pool fullTxOut _)) (Confirmed txOut (AnyOrder _ order)) PoolActions{..} =
  case order of
    DepositAction deposit -> runDeposit (Confirmed txOut deposit) (fullTxOut, pool)
    RedeemAction redeem   -> runRedeem (Confirmed txOut redeem) (fullTxOut, pool)
    SwapAction swap       -> runSwap (Confirmed txOut swap) (fullTxOut, pool)