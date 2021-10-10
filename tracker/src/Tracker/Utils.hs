module Tracker.Utils 
    ( unsafeFromEither,
      toFullTxOut
    ) where

import RIO
import Tracker.Models.ExplorerModels
import Dex.Models
import Plutus.V1.Ledger.Address 
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Scripts  (ValidatorHash(..))
import Data.ByteString.Char8 as Data

unsafeFromEither :: Either String a -> a
unsafeFromEither (Left err)    = error err
unsafeFromEither (Right value) = value

toFullTxOut :: ApiFullTxOut -> FullTxOut
toFullTxOut ApiFullTxOut{..} = 
  FullTxOut {
    outGId           = outGId,
    refId            = refId,
    refIdx           = refIdx, -- ^ Index into the referenced transaction's outputs
    txOutAddress     = scriptHashAddress (ValidatorHash $ Data.pack txOutAddress),
    txOutValue       = jsValue,
    fullTxOutDatum   = datum
  }
