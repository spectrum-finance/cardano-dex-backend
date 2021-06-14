module Dex.Contract.Integration 
     ( checkTxOutForProxyContract
     , checkTxOutForAmmContract 
     ) where

import RIO
import Plutus.V1.Ledger.Tx ( TxOut(..) )

checkTxOutForProxyContract :: TxOut -> Bool
checkTxOutForProxyContract _ = True

checkTxOutForAmmContract :: TxOut -> Bool
checkTxOutForAmmContract _ = True
