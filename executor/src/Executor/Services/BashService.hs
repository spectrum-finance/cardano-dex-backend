module Executor.Services.BashService where

import Plutus.V1.Ledger.Tx
import Data.Set as Set

-- todo:
-- 1. 
mkTxBody :: Tx -> IO ()
mkTxBody Tx {..} = do
    let txIn1@TxIn{..} = (Set.elemAt 0 txInputs)
        txIn2@TxIn{..} = (Set.elemAt 1 txInputs)
        inputHashAddr1 = txOutRefId txIn1 ++ txOutRefIdx txIn1
        inputHashAddr2 = txOutRefId txIn2 ++ txOutRefIdx txIn2
        txOutPoolAddr = txIn1 txOutAddress
        txOutUserAddr = txIn2 txOutAddress
        changeAddress = txIn1 txOutAddress

proxyContractAddress :: String
proxyContractAddress = "addr_test1wr0rynymwsydp73a7hcggz4f0w4dllfquw3plczc93ngyeqenvwla"

proxyDatumHash :: String
proxyDatumHash = "20c590f3a566d9552c4a3901ec1bf359a21097d2f4e190402a247362c0b37ba4"

$CARDANO_CLI transaction build \
--tx-in c4ea4a41fe3bc9c95a9a22065345f1835e720f6edda5905b78c1f8b998dfceb1#2 \
--tx-in c4ea4a41fe3bc9c95a9a22065345f1835e720f6edda5905b78c1f8b998dfceb1#0 \
--tx-out addr_test1wr0rynymwsydp73a7hcggz4f0w4dllfquw3plczc93ngyeqenvwla+1689618+1000+"1000 a0ae9ff88ab1a59117891f1464064841c511a6f34a968c5320769fed.tima" \
--tx-out-datum-hash 20c590f3a566d9552c4a3901ec1bf359a21097d2f4e190402a247362c0b37ba4 \
--tx-out addr_test1qquq5whv5jnw8mnmae5emvzyd3c3hwgqs5sce444wahdf8naexwnl020a0mdl4gptqpyaffeytduw6r62mj7rc93jfvqgufpvk+99997000+99998000+99998000+"99997000 a0ae9ff88ab1a59117891f1464064841c511a6f34a968c5320769fed.tima"+"99998000 a0ae9ff88ab1a59117891f1464064841c511a6f34a968c5320769fed.sasha"+"99999000 a0ae9ff88ab1a59117891f1464064841c511a6f34a968c5320769fed.timaLP" \
--change-address=addr_test1qquq5whv5jnw8mnmae5emvzyd3c3hwgqs5sce444wahdf8naexwnl020a0mdl4gptqpyaffeytduw6r62mj7rc93jfvqgufpvk \
--testnet-magic 8 \
--out-file tx.build \
--alonzo-era