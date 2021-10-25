module Tracker.Utils 
    ( unsafeFromEither
    , toFullTxOut
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
import Cardano.Models as Sdk
import Ledger.Tx
import PlutusTx.Builtins.Internal

import Explorer.Models as Explorer

unsafeFromEither :: Either String a -> a
unsafeFromEither (Left err)    = Prelude.error err
unsafeFromEither (Right value) = value

toFullTxOut :: Explorer.FullTxOut -> Sdk.FullTxOut
toFullTxOut Explorer.FullTxOut{..} = undefined -- do
  -- let value = Value $ AssocMap.fromList $ List.map (\OutputAsset{..} -> (CurrencySymbol $ BuiltinByteString $ DataC.pack policy, AssocMap.singleton (TokenName $ BuiltinByteString $ DataC.pack name) quantity)) assets
  --     (refId', refIdxRaw) = List.span (/= ':') ref
  --     refIdx' = List.drop 1 refIdxRaw
  --     res = FullTxOut 
  --       { fullTxOutGix     = Gix index
  --       , fullTxOutRef     = TxOutRef (TxId $ BuiltinByteString $ DataC.pack refId') (read refIdx' :: Integer)
  --       , fullTxOutAddress = scriptHashAddress (ValidatorHash $ BuiltinByteString $ DataC.pack addr)
  --       , fullTxOutValue   = value
  --       , fullTxOutDatum   = data'
  --       }
  -- pure res