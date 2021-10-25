module Executor.Services.Processor
    ( Processor(..)
    , mkProcessor
    ) where

import Executor.Clients.PoolsResolverClient
import Executor.Utils
import Executor.Services.BashService

import RIO
import Prelude (print)

import ErgoDex.State 
import ErgoDex.Amm.Pool
import ErgoDex.Amm.Orders
import ErgoDex.Amm.PoolActions
import Cardano.Models

data Processor = Processor
  { process :: Confirmed AnyOrder -> IO () }

mkProcessor :: PoolActions -> BashService -> PoolsResolverClient  -> Processor
mkProcessor p b h = Processor $ process' p b h

process' :: PoolActions -> BashService -> PoolsResolverClient -> Confirmed AnyOrder -> IO ()
process' p BashService{..} r@PoolsResolverClient{..} c = do
    execRes <- executeOp r c p
    let (txCandidate, predictedPool) = unsafeFromEither execRes
    print $ "Execute order res is:" ++ show txCandidate
    print $ "Predicted pool is:" ++ show predictedPool
    sendPredicted predictedPool
    submit txCandidate

executeOp :: PoolsResolverClient -> Confirmed AnyOrder -> PoolActions -> IO (Either OrderExecErr (TxCandidate, Predicted Pool))
executeOp PoolsResolverClient{..} (Confirmed txOut (AnyOrder _ order)) PoolActions{..} =
  case order of
    (DepositAction d@Deposit{..}) -> do
      currentPoolMaybe <- resolvePoolReq depositPoolId
      let currentPool = unsafeFromMaybe currentPoolMaybe
          unsafeRes   = runDeposit (Confirmed txOut d) currentPool
      pure unsafeRes

    (RedeemAction r@Redeem{..}) -> do
      currentPoolMaybe <- resolvePoolReq redeemPoolId
      let currentPool = unsafeFromMaybe currentPoolMaybe
          unsafeRes   = runRedeem (Confirmed txOut r) currentPool
      pure unsafeRes
        
    (SwapAction s@Swap{..}) -> do
      currentPoolMaybe <- resolvePoolReq swapPoolId
      let currentPool = unsafeFromMaybe currentPoolMaybe
          unsafeRes   = runSwap (Confirmed txOut s) currentPool
      pure unsafeRes
                