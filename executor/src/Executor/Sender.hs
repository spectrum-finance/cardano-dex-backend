module Executor.Sender 
    ( SenderService(..)
    , mkSenderService
    ) where

data SenderService env = SenderService
    { send :: Tx -> IO () }

mkSenderService :: SenderService
mkSenderService = SenderService send'

send' :: Tx -> IO ()
send' = undefined