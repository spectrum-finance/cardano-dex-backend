module Executor.Services.OrdersExecutor
  ( OrdersExecutor(..)
  , mkOrdersExecutor
  ) where

import Core.Throw.Combinators
import Executor.Services.PoolsResolver
import Executor.Models.Errors

import RIO

import ErgoDex.State 
import ErgoDex.Amm.Pool
import ErgoDex.Amm.Orders
import ErgoDex.Amm.PoolActions
import CardanoTx.Models
import Core.Types
import SubmitAPI.Service

import qualified CardanoTx.Interop as Interop
import qualified Ledger            as P

data OrdersExecutor f = OrdersExecutor
  { process :: Confirmed AnyOrder -> f ()
  }

mkOrdersExecutor
  :: MonadThrow f
  => PoolActions
  -> PoolsResolver f 
  -> Transactions f era
  -> OrdersExecutor f
mkOrdersExecutor actions resolver txs = OrdersExecutor $ process' actions resolver txs

process'
  :: MonadThrow f
  => PoolActions 
  -> PoolsResolver f
  -> Transactions f era
  -> Confirmed AnyOrder
  -> f ()
process' poolActions PoolsResolver{..} Transactions{..} confirmedOrder@(Confirmed _ (AnyOrder poolId _)) = do
  maybePool <- resolvePool poolId

  pool@(ConfirmedPool confirmedPool) <- throwMaybe EmptyPoolErr maybePool
  (txCandidate, Predicted cout pool) <- throwEither $ runOrder pool confirmedOrder poolActions

  tx <- finalizeTx txCandidate
  let
    fout = mkFullTxOut poolOutRef cout
      where
        txId       = Interop.extractCardanoTxId tx
        poolOutRef = P.TxOutRef txId 0 -- todo: magic num
    ppool = PredictedPool $ OnChainIndexedEntity pool fout (lastConfirmedOutGix confirmedPool)

  _ <- sendPredicted ppool
  submitTx tx

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
