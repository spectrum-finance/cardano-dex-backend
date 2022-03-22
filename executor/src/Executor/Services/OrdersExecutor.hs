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

data OrdersExecutor f = OrdersExecutor
  { process :: Confirmed AnyOrder -> f () 
  }

mkOrdersExecutor 
  :: (MonadThrow f)
  => PoolActions
  -> PoolsResolver f 
  -> OrdersExecutor f
mkOrdersExecutor pa pr = OrdersExecutor $ process' pa pr

process' 
  :: (MonadThrow f)
  => PoolActions 
  -> PoolsResolver f
  -> Confirmed AnyOrder
  -> f ()
process' poolActions PoolsResolver{..} confirmedOrder@(Confirmed _ (AnyOrder poolId _)) = do
  maybePool <- resolvePool poolId
  pool      <- throwMaybe EmptyPoolErr maybePool
  let 
    maybeTx = runOrder pool confirmedOrder poolActions
  
  (_, Predicted _ ppool) <- throwEither maybeTx

  -- todo: mk FullTxOut (in submit api), then mk OnChainIndexedEntity, then submit to pools-resolver
  -- let
    -- predictedPool = OnChainIndexedEntity ppool 

  -- sendPredicted predictedPool
  -- submit txCandidate
  pure ()

runOrder
  :: ConfirmedPool
  -> Confirmed AnyOrder 
  -> PoolActions 
  -> Either OrderExecErr (TxCandidate, Predicted Pool)
runOrder (OnChainIndexedEntity pool fullTxOut _) (Confirmed txOut (AnyOrder _ order)) PoolActions{..} =
  case order of
    DepositAction deposit -> runDeposit (Confirmed txOut deposit) (Confirmed fullTxOut pool)
    RedeemAction redeem   -> runRedeem (Confirmed txOut redeem) (Confirmed fullTxOut pool)
    SwapAction swap       -> runSwap (Confirmed txOut swap) (Confirmed fullTxOut pool)