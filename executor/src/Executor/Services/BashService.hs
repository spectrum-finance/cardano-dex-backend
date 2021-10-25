module Executor.Services.BashService
    ( mkBashService
    , BashService(..)
    ) where

import qualified PlutusTx.AssocMap as AssocMap
import           Plutus.V1.Ledger.Tx
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Address
import           Plutus.V1.Ledger.Credential
import           Ledger.Scripts

import qualified Data.Map as Map
import           RIO
import           Prelude (print)
import           System.Process
import qualified Cardano.Api as Script
import qualified Data.Text   as Data

import           Cardano.Address
import           Cardano.Models
import           ErgoDex.Amm.Scripts
import           ErgoDex.OffChain

import           Executor.Models.Settings 


data BashService = BashService
  { submit :: TxCandidate -> IO ()
  }
 -- todo rename as 'render'
 -- todo add change addr from cfg
 -- todo add files path from cfg
 -- todo fee

mkBashService :: PaymentSettings -> BashService
mkBashService p = BashService $ submit' p

submit' :: PaymentSettings -> TxCandidate -> IO ()
submit' s txCandidate = do
  let txBody = mkTxBody' s txCandidate
  _ <- print txBody
  _ <- readProcess txBody [] ""
  _ <- print renderSignScript
  _ <- readProcess renderSignScript [] ""
  _ <- print renderSubmitScript
  void $ readProcess renderSubmitScript [] ""

mkTxBody' :: PaymentSettings -> TxCandidate -> String
mkTxBody' PaymentSettings{..} TxCandidate{..} =
    "$CARDANO_CLI transaction build \\n" ++ txInScript ++ txOutScript ++
      "--change-address=" ++ (Data.unpack feeAddr) ++ "\\n" ++
      "--testnet-magic 8 \\n --out-file tx.build \\n --alonzo-era"
  where
    txInScript  = mkTxInScript `RIO.concatMap` txCandidateInputs
    txOutScript = renderTxOut `RIO.concatMap` txCandidateOutputs

mkTxInScript :: FullTxIn -> String
mkTxInScript (FullTxIn (FullTxOut _ TxOutRef{..} _ _ _) _ _) =
  "--tx-in " ++ show txOutRefId ++ "#" ++ show txOutRefIdx ++ "\\n"

renderTxOut :: TxOutCandidate -> String
renderTxOut TxOutCandidate{..} =
    "--tx-out " ++ address ++ value ++ datumHashV
  where
    address    = Data.unpack $ mkTxOutAddress txOutCandidateAddress
    value      = mkTxOutValue txOutCandidateValue
    datumHashV = renderTxOutDatumHash txOutCandidateDatum

renderTxOutDatumHash :: Maybe Datum -> String
renderTxOutDatumHash hash =
  case hash of
    Just v -> "--tx-out-datum-hash " ++ show v ++ "\\n"
    _      -> ""
  
mkTxOutAddress :: Address -> Text
mkTxOutAddress (Address (ScriptCredential hash) _)
  | hash == validatorHash poolScript    = renderToShellyAddress Script.Mainnet poolInstance
  | hash == validatorHash swapScript    = renderToShellyAddress Script.Mainnet swapInstance
  | hash == validatorHash depositScript = renderToShellyAddress Script.Mainnet depositInstance
  | otherwise                           = renderToShellyAddress Script.Mainnet redeemInstance

mkTxOutValue :: Value -> String
mkTxOutValue (Value inValue) = Map.foldrWithKey (\k v acc -> acc ++ renderTxOutValue k v ) "" (asDataMap inValue) 

renderTxOutValue :: CurrencySymbol -> AssocMap.Map TokenName Integer -> String
renderTxOutValue policy inValue =
      tokenValue ++ policies
    where
      dataMap    = asDataMap inValue
      tokenValue = RIO.concatMap (\i -> show i ++ "+") dataMap
      policies   = Map.foldrWithKey (\tn i acc -> acc ++ "\"" ++ show i ++ " " ++ show policy ++ "." ++ show tn ++ "\"" ++ "+") "" dataMap

asDataMap :: forall a b . Ord a => AssocMap.Map a b -> Map.Map a b
asDataMap input = Map.fromList $ AssocMap.toList input

renderSignScript :: String
renderSignScript = 
  "$CARDANO_CLI transaction sign \\n" ++
    "--tx-body-file tx.build \\n" ++
    "--signing-key-file ./wallets/wallet2.skey \\n" ++
    "--testnet-magic 8 \\n" ++
    "--out-file tx.signed"

renderSubmitScript :: String
renderSubmitScript = "$CARDANO_CLI transaction submit --tx-file tx.signed --testnet-magic 8"