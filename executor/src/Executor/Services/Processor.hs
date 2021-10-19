module Executor.Services.Processor
    ( Processor(..)
    , mkProcessor
    ) where

import RIO
import Executor.Services.HttpReqService
import Executor.Utils
import Plutus.V1.Ledger.Tx
import Prelude (print)
import Data.Aeson
import Ledger.Constraints.OffChain
import Executor.Services.BashService
import ErgoDex.Amm.Pool
import ErgoDex.Amm.Orders
import ErgoDex.Amm.PoolActions
import Cardano.Models
import  ErgoDex.State 

data Processor = Processor
    { process :: AnyOrder -> IO () }

mkProcessor :: PoolActions -> BashService -> HttpReqService  -> Processor
mkProcessor p b h = Processor $ process' p b h

process' :: PoolActions -> BashService -> HttpReqService -> AnyOrder -> IO ()
process' p BashService{..} r@HttpReqService{..} (AnyOrder _ order) = do
    (pool, tx) <- mkTxPool r order p
    print $ "Pool is: " ++ show pool
    print $ encode $ unsafeFromEither tx
    sendPredicted pool  
    mkTxBody (unsafeFromEither tx) pool


mkTxPool :: HttpReqService -> OrderAction a -> PoolActions -> IO (Either OrderExecErr (TxCandidate, Predicted Pool))
mkTxPool HttpReqService{..} op PoolActions{..} =
        case op of
            x@ (DepositAction r) -> do
                currentPoolMaybe <- resolvePoolReq (depositPoolId  r)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = runDeposit (Confirmed x) currentPool
                pure $ (currentPool, unsafeTx)
            x@ (RedeemAction r) -> do
                currentPoolMaybe <- resolvePoolReq (redeemPoolId r)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = runRedeem (Confirmed x) currentPool
                pure $ (currentPool, unsafeTx)
                
            x@ (SwapAction r) -> do
                currentPoolMaybe <- resolvePoolReq (swapPoolId r)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = runSwap (Confirmed x) currentPool
                pure $ (currentPool, unsafeTx)
                