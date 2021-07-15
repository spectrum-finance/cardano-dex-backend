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
import Ledger.Constraints.OffChain

data Processor = Processor
    { process :: ParsedOperation -> IO () }

mkProcessor :: SenderService -> HttpReqService -> InterpreterService -> Processor
mkProcessor s h i = Processor $ process' s h i

process' :: SenderService -> HttpReqService -> InterpreterService -> ParsedOperation -> IO ()
process'  SenderService{..} HttpReqService{..} i (ParsedOperation op) = do
    currentPoolMaybe <- resolvePoolReq
    let currentPool = unsafeFromMaybe currentPoolMaybe
        unsafeTx = unsafeFromEither $ mkTxF i op currentPool
    send unsafeTx

mkTxF :: InterpreterService -> Operation a -> Pool -> Either MkTxError Tx
mkTxF InterpreterService{..} op pool =
        case op of
            x@ (SwapOperation _) -> deposit x pool
            x@ (DepositOperation _) -> redeem x pool
            x@ (RedeemOperation _) -> swap x pool