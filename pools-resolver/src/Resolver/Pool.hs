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
import RIO.ByteString as S
import Dex.Models (Pool)
import Data.Aeson as Json
import Database.Redis as Redis
import Prelude (print)
import Data.ByteString.UTF8 as BSU
import RIO.ByteString.Lazy as LBS
import Resolver.Utils

mkLastPredictedKey :: PoolId -> BSU.ByteString
mkLastPredictedKey (PoolId id) = BSU.fromString $ "last_predicted_" ++ (show id)

mkLastConfirmedKey :: PoolId -> BSU.ByteString
mkLastConfirmedKey (PoolId id) = BSU.fromString $ "last_confirmed_" ++ (show id)

mkPredictedNext :: PoolId -> TxId -> Integer -> BSU.ByteString
mkPredictedNext (PoolId id) (TxId txId) gix = BSU.fromString $ "predicted_next_" ++ (show txId) ++ (show gix) ++ "_" ++ (show id)

putPredicted :: PredictedPool Pool -> IO ()
putPredicted (PredictedPool pool) = do
    conn <- checkedConnect defaultConnectInfo
    res <- runRedis conn $ do
        let pIdLast = poolId $ poolData pool
            predictedNext =  mkPredictedNext pIdLast (txOutRefId $ fullTxOut pool) (gIdx $ gId pool)
            predictedLast =  mkLastPredictedKey pIdLast
            encodedPool = (BS.toStrict . encode) pool
        Redis.set predictedNext encodedPool
        Redis.set predictedLast encodedPool
    _ <- print res
    pure ()

putConfirmed :: ConfirmedPool Pool -> IO ()
putConfirmed (ConfirmedPool pool) = do
    conn <- checkedConnect defaultConnectInfo
    res <- runRedis conn $ do
        let pIdConfirmed = poolId $ poolData pool
            confirmed = mkLastConfirmedKey pIdConfirmed
            encodedPool = (BS.toStrict . encode) pool
        Redis.set confirmed encodedPool
    _ <- print res
    pure ()

getLastPredicted :: PoolId -> IO (Maybe (PredictedPool Pool))
getLastPredicted pIdLast = do
    conn <- checkedConnect defaultConnectInfo
    res <- runRedis conn $ do
        Redis.get $ mkLastPredictedKey pIdLast
    _ <- print res
    let resParsed = (unsafeFromEither res) >>= (\s -> (Json.decode $ LBS.fromStrict s) :: Maybe Pool)
    pure (fmap (\pool -> PredictedPool pool) resParsed)

getLastConfirmed :: PoolId -> IO (Maybe (ConfirmedPool Pool))
getLastConfirmed pIdConfirmed = do
    conn <- checkedConnect defaultConnectInfo
    res <- runRedis conn $ do
        Redis.get $ mkLastConfirmedKey pIdConfirmed
    _ <- print res
    let resParsed = (unsafeFromEither res) >>= (\s -> (Json.decode $ LBS.fromStrict s) :: Maybe Pool)
    pure (fmap (\pool -> ConfirmedPool pool) resParsed) 

existsPredicted :: PoolId -> TxId -> Integer -> IO Bool
existsPredicted pId txId gix = do
    conn <- checkedConnect defaultConnectInfo
    res <- runRedis conn $ do
        Redis.exists $ mkPredictedNext pId txId gix
    pure (unsafeFromEither res)