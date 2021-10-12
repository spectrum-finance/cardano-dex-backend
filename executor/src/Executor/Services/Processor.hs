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
import ErgoDex.InterpreterService
import ErgoDex.Amm.Orders
import ErgoDex.Amm.Pool

data Processor = Processor
    { process :: OrderAction a -> IO () }

mkProcessor :: BashService -> HttpReqService -> InterpreterService -> Processor
mkProcessor b h i = Processor $ process' b h i

process' :: BashService -> HttpReqService -> InterpreterService -> OrderAction a -> IO ()
process' BashService{..} r@HttpReqService{..} i op = do
    (pool, tx) <- mkTxPool i r op
    print $ "Pool is: " ++ show pool
    print $ encode $ unsafeFromEither tx
    sendPredicted pool  
    mkTxBody (unsafeFromEither tx) pool


mkTxPool :: InterpreterService -> HttpReqService-> OrderAction a -> IO (Pool, Either Err Tx)
mkTxPool InterpreterService{..} HttpReqService{..} op =
        case op of
            x@ (Deposit r) -> do
                currentPoolMaybe <- resolvePoolReq (depositPoolId  r)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = deposit x currentPool
                pure (currentPool, unsafeTx)
            x@ (Redeem r) -> do
                currentPoolMaybe <- resolvePoolReq (redeemPoolId r)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = redeem x currentPool
                pure (currentPool, unsafeTx)
            x@ (Swap r) -> do
                currentPoolMaybe <- resolvePoolReq (swapPoolId r)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = swap x currentPool
                pure (currentPool, unsafeTx)
                