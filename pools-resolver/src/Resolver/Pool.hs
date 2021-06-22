module Resolver.Pool 
    ( putPredicted
    , putConfirmed
    , getLastPredicted
    , getLastConfirmed
    , existsPredicted
    ) where

import RIO
import Resolver.Models.CfmmPool

mkLastPredictedKey :: PoolId -> String
mkLastPredictedKey _ = undefined

mkLastConfirmedKey :: PoolId -> String
mkLastConfirmedKey _ = undefined

mkPredictedNext :: PoolId -> TxOutId -> String
mkPredictedNext _ _ = undefined

putPredicted :: CfmmPool -> IO ()
putPredicted _ = undefined 

putConfirmed :: CfmmPool -> IO ()
putConfirmed _ = undefined

getLastPredicted :: PoolId -> IO Maybe CfmmPool
getLastPredicted _ = undefined 

getLastConfirmed :: PoolId -> IO Maybe CfmmPool
getLastConfirmed _ = undefined 

existsPredicted :: PoolId -> TxOutId -> IO Bool
existsPredicted _ _ = undefined