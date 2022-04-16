module Executor.Services.OrdersExecutor
  ( OrdersExecutor(..)
  , mkOrdersExecutor
  ) where

import Core.Throw.Combinators
import Executor.Services.PoolsResolver
import Tracker.Services.Logger as Log
import SubmitAPI.Internal.Transaction
import Executor.Models.Errors

import RIO

import Debug.Trace
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
  :: (MonadIO f, MonadThrow f, MonadUnliftIO f)
  => PoolActions
  -> PoolsResolver f 
  -> Transactions f era
  -> OrdersExecutor f
mkOrdersExecutor actions resolver txs = OrdersExecutor $ process' actions resolver txs

process'
  :: (MonadIO f, MonadThrow f, MonadUnliftIO f)
  => PoolActions
  -> PoolsResolver f
  -> Transactions f era
  -> Confirmed AnyOrder
  -> f ()
process' poolActions resolver tx confirmedOrder@(Confirmed _ (AnyOrder poolId _)) =
  catch (process'' poolActions resolver tx confirmedOrder) (\(err :: SomeException) ->
    Log.log ("Ignore error during processing order for pool:" ++ (show poolId) ++ ". Err: " ++ (show err))
  )

process''
  :: (MonadIO f, MonadThrow f)
  => PoolActions 
  -> PoolsResolver f
  -> Transactions f era
  -> Confirmed AnyOrder
  -> f ()
process'' poolActions PoolsResolver{..} Transactions{..} confirmedOrder@(Confirmed _ (AnyOrder poolId _)) = do
  _ <- Log.log ("Going to process order for pool: " ++ (show poolId))
  maybePool <- resolvePool poolId
  _ <- Log.log ("Pool resolve result: " ++ (show $ not (isNothing maybePool)))
  pool@(ConfirmedPool confirmedPool) <- throwMaybe EmptyPoolErr maybePool
  (txCandidate, Predicted cout pool) <- throwEither $ runOrder pool confirmedOrder poolActions
  tx <- finalizeTx txCandidate
  let
    fout = mkFullTxOut poolOutRef cout
      where
        txId       = Interop.extractCardanoTxId tx
        poolOutRef = P.TxOutRef txId 0 -- todo: magic num
    ppool = PredictedPool $ OnChainIndexedEntity pool fout (lastConfirmedOutGix confirmedPool)
  _ <- Log.log ("Going to submit new predicted pool with id:" ++ (show poolId))
  _ <- sendPredicted ppool
  _ <- Log.log ("Going to submit tx")
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
