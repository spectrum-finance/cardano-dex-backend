{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Wallet.Helper where

import qualified Cardano.Api as C
import Data.Text 
  ( pack )

import WalletAPI.TrustStore 
  ( importTrustStoreFromCardano, SecretFile (SecretFile), KeyPass (KeyPass) )

runApp :: [String] -> IO ()
runApp args = do
  case args of
    [containerPath, keyPath, pass] -> do
        importTrustStoreFromCardano @_ @C.PaymentKey C.AsPaymentKey (SecretFile containerPath) keyPath (KeyPass . pack $ pass)
        print ("Cyphered storage created succesfully!" :: [Char])
    _ -> 
        error "Incorrect arguments qty. Arguments should be in next order: Container_Path Private_Key_Path Password"