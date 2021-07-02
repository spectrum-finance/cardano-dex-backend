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
import qualified PlutusTx.Builtins as Builtins
import RIO.ByteString.Lazy as BS

mkLastPredictedKey :: PoolId -> String
mkLastPredictedKey (PoolId id) = "last_predicted_" ++ "box_id"--Builtins.decodeUtf8 id

mkLastConfirmedKey :: PoolId -> String
mkLastConfirmedKey (PoolId id) = "last_confirmed_" ++ "box_id" --Builtins.decodeUtf8 id

mkPredictedNext :: PoolId -> TxId -> Integer -> String
mkPredictedNext (PoolId id) (TxId txId) gix = "predicted_next_" ++ (show txId) ++ (show gix) ++ "_" ++ "box_id" --Builtins.decodeUtf8 id

putPredicted :: PredictedPool Pool -> IO ()
putPredicted (PredictedPool pool) = do
     conn <- checkedConnect defaultConnectInfo
     runRedis conn $ do
         set (mkLastConfirmedKey $ poolId pool) (BS.toStrict . encode pool)
         set (mkPredictedNext $ poolId pool) (BS.toStrict . encode pool)

putConfirmed :: ConfirmedPool Pool -> IO ()
putConfirmed _ = undefined

getLastPredicted :: PoolId -> IO (Maybe (PredictedPool Pool))
getLastPredicted _ = undefined 

getLastConfirmed :: PoolId -> IO (Maybe (ConfirmedPool Pool))
getLastConfirmed _ = undefined 

existsPredicted :: PoolId -> TxId -> Integer -> IO Bool
existsPredicted _ _ = undefined