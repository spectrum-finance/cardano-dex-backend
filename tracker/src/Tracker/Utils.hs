{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Tracker.Utils
    ( unsafeFromEither
    , toFullTxOut
    ) where

import CardanoTx.Models  as Sdk
import CardanoTx.Address
import Explorer.Models   as Explorer
import Cardano.Address (fromBech32)
import qualified Cardano.Api.Shelley    as  CS
import Explorer.Types
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.TxId 
import           Plutus.V1.Ledger.Scripts
import           Ledger.Tx.CardanoAPI
import qualified Data.Text.Encoding      as T
import qualified Data.ByteString.Base16  as Hex
import qualified PlutusTx.AssocMap          as AssocMap
import           Plutus.V1.Ledger.Value
import           PlutusTx.Builtins.Internal as PBI
import           Ledger.Tx

import RIO
import Prelude
import Data.ByteString.Char8 as DataC
import RIO.List              as List
import Data.Text             as DataT


unsafeFromOption :: Maybe a -> a
unsafeFromOption Nothing    = Prelude.error "err for nothing"
unsafeFromOption (Just value) = value

toFullTxOut :: Explorer.FullTxOut -> Sdk.FullTxOut
toFullTxOut Explorer.FullTxOut{..} =
    Sdk.FullTxOut 
      { fullTxOutRef       = TxOutRef (TxId $ BuiltinByteString $ mkByteString (unTxHash txHash)) (toInteger index)
      , fullTxOutAddress   = addrC
      , fullTxOutValue     = fullTxOutValue'
      , fullTxOutDatumHash = dataHash
      , fullTxOutDatum     = data'
      }
  where
    txHashS         = DataT.unpack $ unTxHash txHash
    addrC           = unsafeFromOption $ readShellyAddress (unAddr addr)
    fullTxOutValue' = Value $ AssocMap.fromList $ mkValueList value

mkCurrencySymbol :: PolicyId -> CurrencySymbol
mkCurrencySymbol PolicyId{..} = CurrencySymbol $ BuiltinByteString (mkByteString unPolicyId)

mkTokenName :: AssetName -> TokenName
mkTokenName AssetName{..} = TokenName . BuiltinByteString . DataC.pack $ DataT.unpack unAssetName

mkValueList :: [OutAsset] -> [(CurrencySymbol, AssocMap.Map TokenName Integer)]
mkValueList = List.map (\OutAsset{..} -> (mkCurrencySymbol policy, AssocMap.singleton (mkTokenName name) quantity))

mkDatumHash :: Hash32 -> DatumHash
mkDatumHash Hash32{..} = DatumHash . BuiltinByteString . DataC.pack $ DataT.unpack unHash32