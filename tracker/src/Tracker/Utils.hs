module Tracker.Utils 
    ( unsafeFromEither
    , toFullTxOut
    ) where

import CardanoTx.Models  as Sdk
import Explorer.Models as Explorer
import Explorer.Types

import           Plutus.V1.Ledger.Address 
import           Plutus.V1.Ledger.TxId 
import           Plutus.V1.Ledger.Scripts
import qualified PlutusTx.AssocMap          as AssocMap
import           Plutus.V1.Ledger.Value
import           PlutusTx.Builtins.Internal
import           Ledger.Tx

import RIO
import Prelude
import Data.ByteString.Char8 as DataC
import RIO.List              as List


unsafeFromEither :: Either String a -> a
unsafeFromEither (Left err)    = Prelude.error err
unsafeFromEither (Right value) = value

toFullTxOut :: Explorer.FullTxOut -> Sdk.FullTxOut
toFullTxOut Explorer.FullTxOut{..} =
    Sdk.FullTxOut 
      { fullTxOutRef       = TxOutRef (TxId $ BuiltinByteString $ DataC.pack $ unTxHash txHash) (toInteger index)
      , fullTxOutAddress   = scriptHashAddress (ValidatorHash $ BuiltinByteString $ DataC.pack $ unAddr addr)
      , fullTxOutValue     = fullTxOutValue'
      , fullTxOutDatumHash = datumHash
      , fullTxOutDatum     = data'
      }
  where 
    fullTxOutValue' = Value $ AssocMap.fromList $ mkValueList value
    datumHash = fmap mkDatumHash dataHash
    

mkCurrencySymbol :: PolicyId -> CurrencySymbol
mkCurrencySymbol PolicyId{..} = CurrencySymbol . BuiltinByteString . DataC.pack $ unPolicyId

mkTokenName :: AssetName -> TokenName
mkTokenName AssetName{..} = TokenName . BuiltinByteString . DataC.pack $ unAssetName

mkValueList :: [OutAsset] -> [(CurrencySymbol, AssocMap.Map TokenName Integer)]
mkValueList = List.map (\OutAsset{..} -> (mkCurrencySymbol policy, AssocMap.singleton (mkTokenName name) quantity))

mkDatumHash :: Hash32 -> DatumHash
mkDatumHash Hash32{..} = DatumHash . BuiltinByteString . DataC.pack $ unHash32