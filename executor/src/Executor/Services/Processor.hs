module Executor.Services.Processor
    ( Processor(..)
    , mkProcessor
    ) where

import Executor.Utils
import Executor.Models.Errors
import Executor.Services.PoolsResolver

import RIO
import Prelude (print)

import ErgoDex.State 
import ErgoDex.Amm.Pool
import ErgoDex.Amm.Orders
import ErgoDex.Amm.PoolActions
import Cardano.Models

data Processor f = Processor
  { process :: Confirmed AnyOrder -> f () 
  }

mkProcessor :: PoolActions -> PoolsResolver -> Processor
mkProcessor pa pr = Processor $ process' pa pr

process' 
  :: (Monad f, MonadThrow f)
  => PoolActions 
  -> PoolsResolver 
  -> Confirmed AnyOrder
  -> f ()
process' poolActions PoolsResolver{..} confirmedOrder@(Confirmed (AnyOrder poolId _)) = do
  maybePool <- resolvePool poolId

  let
    maybeTx = fmap (\(Confirmed _ pool) -> runOrder pool confirmedOrder poolActions) maybePool
  
  (txCandidate, pool) <- throwMaybe . throwEither maybeTx

  sendPredicted pool
  -- submit txCandidate

runOrder
  :: Pool
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
throwMaybe _ = throwM const MaybeExecption