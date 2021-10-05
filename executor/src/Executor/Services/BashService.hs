module Executor.Services.BashService where

import Plutus.V1.Ledger.Tx
import Data.Set as Set
 
 --todo
 -- 1. Min ada value calculation in pool output
mkTxBody :: Tx -> ErgoDexPool -> IO ()
mkTxBody Tx {..} ErgoDexPool {..} = do
    let swapInput@TxIn{..} = (Set.elemAt 0 txInputs)
        poolInput@TxIn{..} = (Set.elemAt 1 txInputs)
        swapHashAddr = txOutRefId swapInput ++ "#" ++ txOutRefIdx swapInput
        poolHashAddr = txOutRefId poolInput ++ "#" ++ txOutRefIdx poolInput
        swapOutput@TxOut{..} = (Set.elemAt 0 txOutputs)
        poolOutput@TxOut{..} = (Set.elemAt 1 txOutputs)
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
    _ <- println swapValue
    _ <- println poolValue
    let
        bashScript = 
            "$CARDANO_CLI transaction build \\n" ++ 
                "--tx-in " ++ swapHashAddr ++ "\\n" ++
                "--tx-in " ++ poolHashAddr ++ "\\n" ++
                "-tx-out " ++ swapOutputAddr ++ "+" ++ outputSwapValueTokenX ++ "+" ++ outputSwapValueTokenY ++ "+" ++ outputSwapValueTokenLP ++
                    "\" " ++ outputSwapValueTokenX ++ policy ++ ".tima\"+" ++
                    "\" " ++ outputSwapValueTokenY ++ policy ++ ".sasha\"+" ++
                    "\" " ++ outputSwapValueTokenLP ++ policy ++ ".tsLP\"" ++ "\\n" ++
                                                    --    "v - here have to be all ada from prev output "
                "--tx-out" ++ poolAndChangeOutputAddr ++ "+1" ++ "+" ++ outputPoolValueTokenX ++ "+" ++ outputPoolValueTokenY ++ "+" ++ outputPoolValueTokenLP ++
                    "\" " ++ outputPoolValueTokenX ++ policy ++ ".tima\"+" ++
                    "\" " ++ outputPoolValueTokenY ++ policy ++ ".sasha\"+" ++
                    "\" " ++ outputPoolValueTokenLP ++ policy ++ ".tsLP\"" ++ "\\n" ++
                "--change-address=" ++ poolAndChangeOutputAddr ++ "\\n" ++
                "--testnet-magic 8 \\n --out-file tx.build \\n --alonzo-era"
    println bashScript

        
policy :: String
policy = "a0ae9ff88ab1a59117891f1464064841c511a6f34a968c5320769fed"