module Executor.Services.Processor
    ( Processor(..)
    , mkProcessor
    ) where

import RIO
import Dex.Interpreter
import Dex.Models
import Executor.Services.HttpReqService
import Executor.Utils
import Plutus.V1.Ledger.Tx
import Prelude (print)
import Data.Aeson
import Ledger.Constraints.OffChain
import Executor.Services.BashService

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


mkTxPool :: InterpreterService -> HttpReqService-> Operation a -> IO (Pool, Either ProcError Tx)
mkTxPool InterpreterService{..} HttpReqService{..} op =
        case op of
            x@ (DepositOperation r) -> do
                currentPoolMaybe <- resolvePoolReq (depositPoolId  r)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = deposit x currentPool
                pure $ (currentPool, unsafeTx)
            x@ (RedeemOperation r) -> do
                currentPoolMaybe <- resolvePoolReq (redeemPoolId r)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = redeem x currentPool
                pure $ (currentPool, unsafeTx)
                
            x@ (SwapOperation r) -> do
                currentPoolMaybe <- resolvePoolReq (swapPoolId r)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = swap x currentPool
                pure $ (currentPool, unsafeTx)
                