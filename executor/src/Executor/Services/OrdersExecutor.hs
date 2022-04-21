module Executor.Services.OrdersExecutor
  ( OrdersExecutor(..)
  , mkOrdersExecutor
  ) where

import           Core.Throw.Combinators
import           Common.Exception.Catch           as Catch
import qualified Control.Exception                as CE
import qualified UnliftIO.Exception               as UE
import           Executor.Services.PoolsResolver
import           SubmitAPI.Internal.Transaction
import           Executor.Models.Errors

import RIO

import System.Logging.Hlog
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
  :: (Monad i, MonadIO f, MonadThrow f, MonadUnliftIO f)
  => PoolActions
  -> MakeLogging i f
  -> PoolsResolver f 
  -> Transactions f era
  -> i (OrdersExecutor f)
mkOrdersExecutor actions MakeLogging{..} resolver txs = do
  logger <- forComponent "ordersExecutor"
  pure $ OrdersExecutor $ process' actions logger resolver txs

process'
  :: (MonadIO f, MonadThrow f, MonadUnliftIO f)
  => PoolActions
  -> Logging f
  -> PoolsResolver f
  -> Transactions f era
  -> Confirmed AnyOrder
  -> f ()
process' poolActions logging@Logging{..} resolver tx confirmedOrder@(Confirmed _ (AnyOrder poolId _)) =
  Catch.catch @CE.SomeException (process'' poolActions logging resolver tx confirmedOrder) (\err ->
    infoM ("Ignore error during processing order for pool:" ++ (show poolId) ++ ". Err:" ++ (show err))
  )

process''
  :: (MonadIO f, MonadThrow f)
  => PoolActions
  -> Logging f
  -> PoolsResolver f
  -> Transactions f era
  -> Confirmed AnyOrder
  -> f ()
process'' poolActions Logging{..} PoolsResolver{..} Transactions{..} confirmedOrder@(Confirmed txOut (AnyOrder poolId o)) = do
  _ <- infoM ("Going to process order " ++ (show o) ++ " for pool: " ++ (show poolId) ++ ". TxOut:" ++ (show txOut) )
  maybePool <- resolvePool poolId
  _ <- infoM ("Pool resolve result: " ++ (show $ not (isNothing maybePool)))
  pool@(ConfirmedPool confirmedPool) <- throwMaybe EmptyPoolErr maybePool
  (txCandidate, Predicted cout pool) <- throwEither $ runOrder pool confirmedOrder poolActions
  _ <- infoM ("TxCandidate: " ++ (show txCandidate))
  tx <- finalizeTx txCandidate
  let
    fout = mkFullTxOut poolOutRef cout
      where
        txId       = Interop.extractCardanoTxId tx
        poolOutRef = P.TxOutRef txId 0 -- todo: magic num
    ppool = PredictedPool $ OnChainIndexedEntity pool fout (lastConfirmedOutGix confirmedPool)
  _ <- infoM ("Going to submit new predicted pool with id:" ++ (show poolId))
  _ <- sendPredicted ppool
  _ <- infoM @String ("Going to submit tx")
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
