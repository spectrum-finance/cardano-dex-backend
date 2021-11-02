module Executor.Services.OrdersExecutor
    ( OrdersExecutor(..)
    , mkOrdersExecutor
    ) where

import Executor.Models.Errors
import Executor.Services.PoolsResolver

import RIO

import ErgoDex.State 
import ErgoDex.Amm.Pool
import ErgoDex.Amm.Orders
import ErgoDex.Amm.PoolActions
import Cardano.Models

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
  pool      <- throwMaybe maybePool
  let 
    maybeTx = runOrder pool confirmedOrder poolActions
  
  (txCandidate, pool) <- throwEither maybeTx

  sendPredicted pool
  -- submit txCandidate

runOrder
  :: Confirmed Pool
  -> Confirmed AnyOrder 
  -> PoolActions 
  -> Either OrderExecErr (TxCandidate, Predicted Pool)
runOrder pool (Confirmed txOut (AnyOrder _ order)) PoolActions{..} =
  case order of
    DepositAction deposit -> runDeposit (Confirmed txOut deposit) pool
    RedeemAction redeem   -> runRedeem (Confirmed txOut redeem) pool
    SwapAction swap       -> runSwap (Confirmed txOut swap) pool

throwEither :: (MonadThrow f, Exception e) => Either e r -> f r
throwEither (Left err)    = throwM err
throwEither (Right value) = pure value

throwMaybe :: (MonadThrow f) => Maybe a -> f a
throwMaybe (Just value) = pure value
throwMaybe _ = throwM MaybeErr