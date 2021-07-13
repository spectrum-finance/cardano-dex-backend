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

mkProcessor :: SenderService -> HttpClient -> (InterpreterService a b) -> Processor
mkProcessor s h i = Processor $ process' s h i

process' :: SenderService -> HttpClient -> (InterpreterService a b) -> ParsedOperation -> IO ()
process'  SenderService{..} HttpClient{..} InterpreterService{..} (ParsedOperation op) = do
    currentPoolMaybe <- resolvePoolReq
    let currentPool = unsafeFromMaybe currentPoolMaybe
        unsafeTx = mkTx'' op currentPool
    send unsafeTx

mkTx'' :: Operation a -> Pool -> Tx
mkTx'' op pool =
    unsafeFromEither $
        case op of
            x@ (SwapOperation swapData) -> deposit x pool
            x@ (DepositOperation depositData) -> redeem x pool
            x@ (RedeemOperation redeemData) -> swap x pool