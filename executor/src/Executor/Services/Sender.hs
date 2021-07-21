module Executor.Services.Sender 
    ( SenderService(..)
    , mkSenderService
    ) where

import RIO
import Plutus.V1.Ledger.Tx
import Prelude (print)

data SenderService = SenderService
    { send :: Tx -> IO () }

mkSenderService :: SenderService
mkSenderService = SenderService send'

send' :: Tx -> IO () 
send' tx = print "Submittings next tx to the network..." >> pure ()