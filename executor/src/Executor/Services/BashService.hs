module Executor.Services.BashService
    ( mkBashService
    , BashService(..)
    ) where

import Plutus.V1.Ledger.Tx
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import RIO
import Executor.Utils
import Prelude (print)
import Plutus.V1.Ledger.Value
import System.Process
import ErgoDex.Amm.Pool
import Cardano.Models
import Plutus.V1.Ledger.TxId
import Data.Text.Prettyprint.Doc
import qualified Ledger.Typed.Scripts     as Scripts
import Ledger.Scripts
import ErgoDex.Amm.Scripts
import Plutus.V1.Ledger.Address
import Plutus.V1.Ledger.Credential
import ErgoDex.OffChain
import Cardano.Address
import qualified Cardano.Api              as Script
import qualified PlutusTx.AssocMap                as AssocMap
import qualified Data.Text as Data

data BashService = BashService
  { submit :: TxCandidate -> IO ()
  }
 -- todo rename as 'render'
 -- todo add change addr from cfg
 -- todo add files path from cfg
 -- todo fee

mkBashService :: BashService
mkBashService = BashService submit'

submit' :: TxCandidate -> IO ()
submit' txCandidate = do
  let txBody = mkTxBody' txCandidate
  _ <- print txBody
  _ <- readProcess txBody [] ""
  _ <- print renderSignScript
  _ <- readProcess renderSignScript [] ""
  _ <- print renderSubmitScript
  void $ readProcess renderSubmitScript [] ""

mkTxBody' :: TxCandidate -> String
mkTxBody' TxCandidate{..} =
    "$CARDANO_CLI transaction build \\n" ++ txInScript ++ txOutScript ++
      "--change-address=" ++ "" ++ "\\n" ++
      "--testnet-magic 8 \\n --out-file tx.build \\n --alonzo-era"
  where
    txInScript = mkTxInScript `RIO.concatMap` txCandidateInputs
    txOutScript = renderTxOut `RIO.concatMap` txCandidateOutputs

mkTxInScript :: FullTxIn -> String
mkTxInScript (FullTxIn (FullTxOut _ TxOutRef{..} _ _ _) _ _) =
  "--tx-in " ++ show txOutRefId ++ "#" ++ show txOutRefIdx ++ "\\n"

renderTxOut :: TxOutCandidate -> String
renderTxOut TxOutCandidate{..} =
    "--tx-out " ++ address ++ value ++ datumHash
  where
    address = Data.unpack $ mkTxOutAddress txOutCandidateAddress
    value = mkTxOutValue txOutCandidateValue
    datumHash = renderTxOutDatumHash txOutCandidateDatum

renderTxOutDatumHash :: Maybe Datum -> String
renderTxOutDatumHash hash =
  case hash of
    Just v -> "--tx-out-datum-hash " ++ show v ++ "\\n"
    _ -> ""
  
mkTxOutAddress :: Address -> Text
mkTxOutAddress (Address (ScriptCredential hash) _)
  | hash == validatorHash poolScript = renderToShellyAddress Script.Mainnet poolInstance
  | hash == validatorHash swapScript = renderToShellyAddress Script.Mainnet swapInstance
  | hash == validatorHash depositScript = renderToShellyAddress Script.Mainnet depositInstance
  | otherwise = renderToShellyAddress Script.Mainnet redeemInstance

isPool :: Address -> Bool
isPool (Address (ScriptCredential hash) _)
  | hash == validatorHash poolScript = True 
  | otherwise = False

mkTxOutValue :: Value -> String
mkTxOutValue (Value inValue) = Map.foldrWithKey (\k v acc -> acc ++ renderTxOutValue k v ) "" (asDataMap inValue) 

renderTxOutValue :: CurrencySymbol -> AssocMap.Map TokenName Integer -> String
renderTxOutValue policy inValue =
      tokenValue ++ policies
    where
      dataMap = asDataMap inValue
      tokenValue = RIO.concatMap (\i -> show i ++ "+") dataMap
      policies = Map.foldrWithKey (\tn i acc -> acc ++ "\"" ++ show i ++ " " ++ show policy ++ "." ++ show tn ++ "\"" ++ "+") "" dataMap

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