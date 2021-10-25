module Tracker.Utils 
    ( unsafeFromEither
    
    ) where

import RIO
import Plutus.V1.Ledger.Address 
import Plutus.V1.Ledger.TxId 
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Scripts
import Data.ByteString.Char8 as DataC
import qualified PlutusTx.AssocMap as AssocMap
import RIO.List as List
import RIO.Map as Map
import Plutus.V1.Ledger.Value
import Prelude as Prelude
import Cardano.Models
import Ledger.Tx
import PlutusTx.Builtins.Internal

unsafeFromEither :: Either String a -> a
unsafeFromEither (Left err)    = Prelude.error err
unsafeFromEither (Right value) = value