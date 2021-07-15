module Resolver.Repositories.PoolRepository
    ( PoolRepository(..)
    , mkPoolRepository
    ) where

import RIO
import Dex.Models
import Resolver.Models.CfmmPool
import Plutus.V1.Ledger.TxId
import RIO.ByteString.Lazy as BS
import Data.Aeson as Json
import Database.Redis as Redis
import Prelude (print)
import Data.ByteString.UTF8 as BSU
import RIO.ByteString.Lazy as LBS
import Resolver.Utils

data PoolRepository = PoolRepository
    { putPredicted :: PredictedPool Pool -> IO ()
    , putConfirmed :: ConfirmedPool Pool -> IO ()
    , getLastPredicted :: PoolId -> IO (Maybe (PredictedPool Pool))
    , getLastConfirmed :: PoolId -> IO (Maybe (ConfirmedPool Pool))
    , existsPredicted :: PoolId -> TxId -> Integer -> IO Bool
    }

mkPoolRepository :: IO PoolRepository
mkPoolRepository = do
    conn <- checkedConnect defaultConnectInfo
    _ <- print "Redis connection established..."
    pure $ PoolRepository (putPredicted' conn) (putConfirmed' conn) (getLastPredicted' conn) (getLastConfirmed' conn) (existsPredicted' conn)

putPredicted' :: Connection -> PredictedPool Pool -> IO ()
putPredicted' conn (PredictedPool pool@Pool{..}) = do 
    res <- runRedis conn $ do
        let predictedNext = mkPredicted (poolId poolData) (refId fullTxOut) (gIdx gId)
            predictedLast = mkLastPredictedKey (poolId poolData)
            encodedPool = (BS.toStrict . encode) pool
        Redis.set predictedNext encodedPool
        Redis.set predictedLast encodedPool
    print res

putConfirmed' :: Connection -> ConfirmedPool Pool -> IO ()
putConfirmed' conn (ConfirmedPool pool@Pool{..}) = do
    res <- runRedis conn $ do
        let confirmed = mkLastConfirmedKey $ poolId poolData
            encodedPool = (BS.toStrict . encode) pool
        Redis.set confirmed encodedPool
    print res

getLastPredicted' :: Connection -> PoolId -> IO (Maybe (PredictedPool Pool))
getLastPredicted' conn pIdLast = do
    res <- runRedis conn $ do
        Redis.get $ mkLastPredictedKey pIdLast
    _ <- print res
    let resParsed = (unsafeFromEither res) >>= (\s -> (Json.decode $ LBS.fromStrict s) :: Maybe Pool)
    pure (fmap (\pool -> PredictedPool pool) resParsed)

getLastConfirmed' :: Connection -> PoolId -> IO (Maybe (ConfirmedPool Pool))
getLastConfirmed' conn pIdConfirmed = do
    res <- runRedis conn $ do
        Redis.get $ mkLastConfirmedKey pIdConfirmed
    _ <- print res
    let resParsed = (unsafeFromEither res) >>= (\s -> (Json.decode $ LBS.fromStrict s) :: Maybe Pool)
    pure (fmap (\pool -> ConfirmedPool pool) resParsed) 

existsPredicted' :: Connection -> PoolId -> TxId -> Integer -> IO Bool
existsPredicted' conn pId txId gix = do
    res <- runRedis conn $ do
        Redis.exists $ mkPredicted pId txId gix
    pure $ unsafeFromEither res

-------------------------------------------------------------------------------------

mkLastPredictedKey :: PoolId -> BSU.ByteString
mkLastPredictedKey (PoolId poolId) = BSU.fromString $ "predicted:last:" ++ (show poolId)

mkLastConfirmedKey :: PoolId -> BSU.ByteString
mkLastConfirmedKey (PoolId poolId) = BSU.fromString $ "confirmed:last:" ++ (show poolId)

mkPredicted :: PoolId -> TxId -> Integer -> BSU.ByteString
mkPredicted (PoolId pid) (TxId txId) gix = BSU.fromString $ "predicted:" ++ (show txId) ++ ":" ++ (show gix) ++ ":" ++ (show pid)