module Executor.Processor
    ( Processor(..)
    , mkProcessor
    ) where

import RIO
import Dex.Interpreter
import Executor.HttpClient
import Executor.Utils

data Processor = Processor
    { process :: ParsedOperation -> IO () }

mkProcessor :: SenderService -> HttpClient -> InterpreterService -> Processor
mkProcessor s h i = Processor $ process' s h i

process' :: SenderService -> HttpClient -> InterpreterService -> ParsedOperation -> IO ()
process'  SenderService{..} InterpreterService{..} HttpClient{..} (ParsedOperation op) = do
    currentPool <- resolvePoolReq
    let tx = unsafeFromEither (case op of
        x@ (SwapOperation swapData) -> swap x currentPool
        x@ (DepositOperation depositData) -> deposit x currentPool
        x@ (RedeemOperation redeemData) -> redeem x currentPool)
    send tx