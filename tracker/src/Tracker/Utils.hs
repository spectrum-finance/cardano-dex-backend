module Tracker.Utils 
    ( unsafeFromEither
    , toFullTxOut
    ) where

import RIO
import Tracker.Models.ExplorerModels
import Dex.Models
import Plutus.V1.Ledger.Address 
import Plutus.V1.Ledger.TxId 
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Scripts
import Data.ByteString.Char8 as DataC
import qualified PlutusTx.AssocMap as AssocMap
import RIO.List as List
import RIO.Map as Map
import Plutus.V1.Ledger.Value
import Prelude

unsafeFromEither :: Either String a -> a
unsafeFromEither (Left err)    = error err
unsafeFromEither (Right value) = value

toFullTxOut :: ApiFullTxOut -> IO FullTxOut
toFullTxOut ApiFullTxOut{..} = do
  let value = Value $ AssocMap.fromList $ List.map (\OutputAsset{..} -> (CurrencySymbol $ DataC.pack policy, AssocMap.singleton (TokenName $ DataC.pack name) quantity)) assets
      (refId', refIdxRaw) = List.span (/= ':') ref
      refIdx' = List.drop 1 refIdxRaw
      res = FullTxOut 
        { outGId         = GId index
        , refId          = TxId $ DataC.pack refId'
        , refIdx         = read refIdx' :: Integer
        , txOutAddress   = scriptHashAddress (ValidatorHash $ DataC.pack addr)
        , txOutValue     = value
        , fullTxOutDatum = unitDatum -- todo: get datum from explorer model
        }
  -- print res
  pure res