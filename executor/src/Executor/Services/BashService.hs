module Executor.Services.BashService
    ( mkBashService
    , BashService(..)
    ) where

import System.FilePath
import Plutus.V1.Ledger.Tx
import Data.Set as Set
import Data.List as List
import RIO
import Executor.Utils
import Prelude (print)
import Plutus.V1.Ledger.Value
import System.Process
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified Plutus.V1.Ledger.Api as PlutusApi
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import           Codec.Serialise
import qualified Cardano.Api.Shelley as Shelley
import           Prelude
import           System.Environment
import ErgoDex.Amm.Pool
import ErgoDex.Contracts.Types
import ErgoDex.OffChain
import qualified Ledger.Typed.Scripts   as Scripts

data BashService = BashService
    { mkTxBody :: Tx -> Pool -> IO ()
    }

mkBashService :: BashService
mkBashService = BashService mkTxBody'

mkScriptAddress' :: IO ()
mkScriptAddress' = do
    let validatorScript = Plutus.unValidatorScript $ Scripts.validatorScript poolInstance
        scriptSBS = SBS.toShort . LBS.toStrict $ serialise validatorScript
        scriptSerial = Shelley.PlutusScriptSerialised scriptSBS
    writePlutusScript 42 "test.plutus" scriptSerial scriptSBS

writePlutusScript :: Integer -> FilePath -> Shelley.PlutusScript Shelley.PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript scriptnum filename scriptSerial scriptSBS =
  do
  case PlutusApi.defaultCostModelParams of
        Just m ->
          let Alonzo.Data pData = Shelley.toAlonzoData (Shelley.ScriptDataNumber scriptnum)
              (logout, e) = PlutusApi.evaluateScriptCounting PlutusApi.Verbose m scriptSBS [pData]
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- Shelley.writeFileTextEnvelope filename Nothing scriptSerial
  case result of
    Left err -> print err
    Right () -> return ()
  
 --todo
 -- 1. ada for fee?
mkTxBody' :: Tx -> Pool -> IO ()
mkTxBody' Tx {..} p@Pool {..} = do
    let swapInput = Set.elemAt 0 txInputs
        poolInput = Set.elemAt 1 txInputs
        swapHashAddr = show ((txOutRefId . txInRef) swapInput) ++ "#" ++ show (txOutRefIdx $ txInRef swapInput)
        poolHashAddr = show ((txOutRefId . txInRef) poolInput) ++ "#" ++ show (txOutRefIdx $ txInRef poolInput)
        swapOutput = List.head txOutputs
        poolOutput = List.last txOutputs
        swapOutputAddr = txOutAddress swapOutput
        poolAndChangeOutputAddr = txOutAddress poolOutput
        poolDatumHash = unsafeFromMaybe $ txOutDatumHash poolOutput
        swapValue = txOutValue swapOutput
        outputSwapValueTokenX = assetClassValueOf swapValue (unCoin poolCoinX)
        outputSwapValueTokenY = assetClassValueOf swapValue (unCoin poolCoinY)
        poolValue = txOutValue poolOutput
        outputPoolValueTokenX = assetClassValueOf poolValue (unCoin poolCoinX)
        outputPoolValueTokenY = assetClassValueOf poolValue (unCoin poolCoinY)
    _ <- print swapValue
    _ <- print poolValue
    let
        mkTxScript = 
            "$CARDANO_CLI transaction build \\n" ++ 
                "--tx-in " ++ swapHashAddr ++ "\\n" ++
                "--tx-in " ++ poolHashAddr ++ "\\n" ++
                "-tx-out " ++ show swapOutputAddr ++ "+" ++ show outputSwapValueTokenX ++ "+" ++ show outputSwapValueTokenY ++
                    "\" " ++ show outputSwapValueTokenX ++ policy ++ ".tima\"+" ++
                    "\" " ++ show outputSwapValueTokenY ++ policy ++ ".sasha\"\\n" ++
                                                        --    "v - here have to be all ada from prev output "
                "--tx-out" ++ show poolAndChangeOutputAddr ++ "+1" ++ "+" ++ show outputPoolValueTokenX ++ "+" ++ show outputPoolValueTokenY ++ "+" ++
                    "\" " ++ show outputPoolValueTokenX ++ policy ++ ".tima\"+" ++
                    "\" " ++ show outputPoolValueTokenY ++ policy ++ ".sasha\"\\n" ++
                "--tx-out-datum-hash" ++ show poolDatumHash ++ "\\n" ++
                "--change-address=" ++ show poolAndChangeOutputAddr ++ "\\n" ++
                "--testnet-magic 8 \\n --out-file tx.build \\n --alonzo-era"
    _ <- print mkTxScript
    void $ readProcess mkTxScript [] ""
    let signScript = 
            "$CARDANO_CLI transaction sign \\n" ++
                "--tx-body-file tx.build \\n" ++
                "--signing-key-file ./wallets/wallet2.skey \\n" ++
                "--testnet-magic 8 \\n" ++
                "--out-file tx.signed"
    _ <- print signScript
    void $ readProcess signScript [] ""
    let submitScript = 
            "$CARDANO_CLI transaction submit --tx-file tx.signed --testnet-magic 8"
    _ <- print submitScript
    void $ readProcess submitScript [] ""

policy :: String
policy = "a0ae9ff88ab1a59117891f1464064841c511a6f34a968c5320769fed"