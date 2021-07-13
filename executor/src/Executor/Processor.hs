module Executor.Processor
    ( Processor(..)
    , mkProcessor
    ) where

import RIO
import Dex.Interpreter
import Dex.Models
import Executor.HttpClient
import Executor.Utils
import Executor.Sender
import Plutus.V1.Ledger.Tx
import Ledger.Constraints.OffChain

data Processor = Processor
    { process :: ParsedOperation -> IO () }

mkProcessor :: SenderService -> HttpClient -> InterpreterService -> Processor
mkProcessor s h i = Processor $ process' s h i

process' :: SenderService -> HttpClient -> InterpreterService -> ParsedOperation -> IO ()
process'  SenderService{..} HttpClient{..} i (ParsedOperation op) = do
    currentPoolMaybe <- resolvePoolReq
    let currentPool = unsafeFromMaybe currentPoolMaybe
        unsafeTx = unsafeFromEither $ mkTx'' i op currentPool
    send unsafeTx

mkTx'' :: InterpreterService -> Operation a -> Pool -> Either MkTxError Tx
mkTx'' InterpreterService{..} op pool =
        case op of
            x@ (SwapOperation swapData) -> deposit x pool
            x@ (DepositOperation depositData) -> redeem x pool
            x@ (RedeemOperation redeemData) -> swap x pool