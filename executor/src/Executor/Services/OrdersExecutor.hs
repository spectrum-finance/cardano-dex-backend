module Executor.Services.OrdersExecutor
  ( OrdersExecutor(..)
  , mkOrdersExecutor
  ) where

import SdkCore.Throw.Combinators
import Core.Extractor.AlonzoTxExtractors
import Core.Extractor.Class
import Executor.Services.PoolsResolver
import Executor.Models.Errors

import RIO

import ErgoDex.State 
import ErgoDex.Amm.Pool
import ErgoDex.Amm.Orders
import ErgoDex.Amm.PoolActions
import CardanoTx.Models
import SubmitAPI.Service
import Core.Types

data OrdersExecutor f = OrdersExecutor
  { process :: Confirmed AnyOrder -> f () 
  }

mkOrdersExecutor 
  :: (MonadThrow f)
  => PoolActions
  -> PoolsResolver f
  -> Transactions f
  -> OrdersExecutor f
mkOrdersExecutor pa pr submitService = OrdersExecutor $ process' pa pr submitService

process' 
  :: (MonadThrow f)
  => PoolActions 
  -> PoolsResolver f
  -> Transactions f
  -> Confirmed AnyOrder
  -> f ()
process' poolActions PoolsResolver{..} Transactions{..} confirmedOrder@(Confirmed _ (AnyOrder poolId _)) = do
  maybePool                      <- resolvePool poolId
  pool@(ConfirmedPool confPool)  <- throwMaybe EmptyPoolErr maybePool
  let
    maybeTx = runOrder pool confirmedOrder poolActions
  (txCandidate, predictedPool)   <- throwEither maybeTx
  finalTx                        <- finalizeTx txCandidate
  let
    mPool = extract finalTx :: Maybe (FullTxOut, Pool)
  (out, pp)                      <- throwMaybe EmptyPoolErr mPool
  let
    predictedPool = PredictedPool $ OnChainIndexedEntity pp out (getGix confPool)
  _                              <- sendPredicted predictedPool
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