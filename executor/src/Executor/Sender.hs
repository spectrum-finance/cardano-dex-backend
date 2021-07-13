module Executor.Sender 
    ( SenderService(..)
    , mkSenderService
    ) where

import RIO
import Plutus.V1.Ledger.Tx

data SenderService = SenderService
    { send :: Tx -> IO () }

mkSenderService :: SenderService
mkSenderService = SenderService send'

send' :: Tx -> IO ()
send' = undefined