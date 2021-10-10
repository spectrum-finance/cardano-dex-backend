module Executor.Services.BashService
    ( mkBashService
    , BashService(..)
    ) where

import Plutus.V1.Ledger.Tx
import Data.Set as Set
import Data.List as List
import Dex.Contract.Models
import RIO
import Executor.Utils
import Prelude (print)
import Plutus.V1.Ledger.Value
import qualified Dex.Models as Dex
import System.Process
import qualified Dex.Models as Dex
import qualified Dex.Models as PD (PoolData(..))

data BashService = BashService
    { mkTxBody :: Tx -> Dex.Pool -> IO ()
    }

mkBashService :: BashService
mkBashService = BashService mkTxBody'

 --todo
 -- 1. Min ada value calculation in pool output
mkTxBody' :: Tx -> Dex.Pool -> IO ()
mkTxBody' Tx {..} p@Dex.Pool {..} = do
    let pdl = Dex.poolData p
        ErgoDexPool {..} = ErgoDexPool (Dex.poolFee pdl) (Dex.xPoolCoin pdl) (Dex.yPoolCoin pdl) (Dex.lpPoolCoin pdl)
        swapInput = Set.elemAt 0 txInputs
        poolInput = Set.elemAt 1 txInputs
        swapHashAddr = show ((txOutRefId . txInRef) swapInput) ++ "#" ++ show (txOutRefIdx $ txInRef swapInput)
        poolHashAddr = show ((txOutRefId . txInRef) poolInput) ++ "#" ++ show (txOutRefIdx $ txInRef poolInput)
        swapOutput = List.head txOutputs
        poolOutput = List.last txOutputs
        swapOutputAddr = txOutAddress swapOutput
        poolAndChangeOutputAddr = txOutAddress poolOutput
        poolDatumHash = unsafeFromMaybe $ txOutDatumHash poolOutput
        swapValue = txOutValue swapOutput
        outputSwapValueTokenX = assetClassValueOf swapValue xCoin
        outputSwapValueTokenY = assetClassValueOf swapValue yCoin
        outputSwapValueTokenLP = assetClassValueOf swapValue lpCoin
        poolValue = txOutValue poolOutput
        outputPoolValueTokenX = assetClassValueOf poolValue xCoin
        outputPoolValueTokenY = assetClassValueOf poolValue yCoin
        outputPoolValueTokenLP = assetClassValueOf poolValue lpCoin
    _ <- print swapValue
    _ <- print poolValue
    let
        mkTxScript = 
            "$CARDANO_CLI transaction build \\n" ++ 
                "--tx-in " ++ swapHashAddr ++ "\\n" ++
                "--tx-in " ++ poolHashAddr ++ "\\n" ++
                "-tx-out " ++ show swapOutputAddr ++ "+" ++ show outputSwapValueTokenX ++ "+" ++ show outputSwapValueTokenY ++ "+" ++ show outputSwapValueTokenLP ++
                    "\" " ++ show outputSwapValueTokenX ++ policy ++ ".tima\"+" ++
                    "\" " ++ show outputSwapValueTokenY ++ policy ++ ".sasha\"+" ++
                    "\" " ++ show outputSwapValueTokenLP ++ policy ++ ".tsLP\"" ++ "\\n" ++
                                                    --    "v - here have to be all ada from prev output "
                "--tx-out" ++ show poolAndChangeOutputAddr ++ "+1" ++ "+" ++ show outputPoolValueTokenX ++ "+" ++ show outputPoolValueTokenY ++ "+" ++ show outputPoolValueTokenLP ++
                    "\" " ++ show outputPoolValueTokenX ++ policy ++ ".tima\"+" ++
                    "\" " ++ show outputPoolValueTokenY ++ policy ++ ".sasha\"+" ++
                    "\" " ++ show outputPoolValueTokenLP ++ policy ++ ".tsLP\"" ++ "\\n" ++
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