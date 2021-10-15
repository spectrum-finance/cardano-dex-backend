module Executor.Services.Processor
    ( Processor(..)
    , ParsedOperation(..)
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

data ParsedOperation = forall a. ParsedOperation { op :: OrderAction a }

data Processor = Processor
    { process :: ParsedOperation -> IO () }

mkProcessor :: BashService -> HttpReqService -> InterpreterService -> Processor
mkProcessor b h i = Processor $ process' b h i

process' :: BashService -> HttpReqService -> InterpreterService -> ParsedOperation -> IO ()
process' BashService{..} r@HttpReqService{..} i (ParsedOperation op) = do
    (pool, tx) <- mkTxPool i r op
    print $ "Pool is: " ++ show pool
    print $ encode $ unsafeFromEither tx
    sendPredicted pool  
    mkTxBody (unsafeFromEither tx) pool


mkTxPool :: InterpreterService -> HttpReqService-> OrderAction a -> IO (Pool, Either Err Tx)
mkTxPool InterpreterService{..} HttpReqService{..} op =
        case op of
            x@ (DepositAction a) -> do
                currentPoolMaybe <- resolvePoolReq (depositPoolId a)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = deposit x currentPool
                pure (currentPool, unsafeTx)
            x@ (RedeemAction a) -> do
                currentPoolMaybe <- resolvePoolReq (redeemPoolId a)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = redeem x currentPool
                pure (currentPool, unsafeTx)
            x@ (SwapAction a) -> do
                currentPoolMaybe <- resolvePoolReq (swapPoolId a)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = swap x currentPool
                pure (currentPool, unsafeTx)
                