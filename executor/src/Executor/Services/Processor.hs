module Executor.Services.Processor
    ( Processor(..)
    , mkProcessor
    ) where

import RIO
import Dex.Interpreter
import Dex.Models
import Executor.Services.HttpReqService
import Executor.Utils
import Executor.Services.Sender
import Plutus.V1.Ledger.Tx
import Prelude (print)
import Data.Aeson
import Ledger.Constraints.OffChain

data Processor = Processor
    { process :: ParsedOperation -> IO () }

mkProcessor :: SenderService -> HttpReqService -> InterpreterService -> Processor
mkProcessor s h i = Processor $ process' s h i

process' :: SenderService -> HttpReqService -> InterpreterService -> ParsedOperation -> IO ()
process'  SenderService{..} r@HttpReqService{..} i (ParsedOperation op) = do
    (pool, tx) <- mkTxPool i r op
    print $ "Pool is: " ++ show pool
    print $ encode tx
    sendPredicted pool    

mkTxPool :: InterpreterService -> HttpReqService-> Operation a -> IO (Pool, Either MkTxError Tx)
mkTxPool InterpreterService{..} HttpReqService{..} op =
        case op of
            x@ (SwapOperation r) -> do
                currentPoolMaybe <- resolvePoolReq (swapPoolId r)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = deposit x currentPool
                pure $ (currentPool, unsafeTx)
            x@ (DepositOperation r) -> do
                currentPoolMaybe <- resolvePoolReq (depositPoolId r)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = redeem x currentPool
                pure $ (currentPool, unsafeTx)
                
            x@ (RedeemOperation r) -> do
                currentPoolMaybe <- resolvePoolReq (redeemPoolId r)
                let currentPool = unsafeFromMaybe currentPoolMaybe
                    unsafeTx = swap x currentPool
                pure $ (currentPool, unsafeTx)
                