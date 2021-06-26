module Resolver.Pool 
    ( putPredicted
    , putConfirmed
    , getLastPredicted
    , getLastConfirmed
    , existsPredicted
    ) where

import RIO
import Dex.Models
import Resolver.Models.CfmmPool
import Plutus.V1.Ledger.TxId

mkLastPredictedKey :: PoolId -> String
mkLastPredictedKey _ = undefined

mkLastConfirmedKey :: PoolId -> String
mkLastConfirmedKey _ = undefined

mkPredictedNext :: PoolId -> TxId -> Integer -> String
mkPredictedNext _ _ = undefined

putPredicted :: PredictedPool Pool -> IO ()
putPredicted _ = undefined 

putConfirmed :: ConfirmedPool Pool -> IO ()
putConfirmed _ = undefined

getLastPredicted :: PoolId -> IO (Maybe (PredictedPool Pool))
getLastPredicted _ = undefined 

getLastConfirmed :: PoolId -> IO (Maybe (ConfirmedPool Pool))
getLastConfirmed _ = undefined 

existsPredicted :: PoolId -> TxId -> Integer -> IO Bool
existsPredicted _ _ = undefined