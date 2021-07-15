module Resolver.Pool 
    ( PoolApi(..)
    , mkPoolApi
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

data PoolApi = PoolApi
    { putPredicted :: PredictedPool Pool -> IO ()
    , putConfirmed :: ConfirmedPool Pool -> IO ()
    , getLastPredicted :: PoolId -> IO (Maybe (PredictedPool Pool))
    , getLastConfirmed :: PoolId -> IO (Maybe (ConfirmedPool Pool))
    , existsPredicted :: PoolId -> TxId -> Integer -> IO Bool
    }

mkPoolApi :: IO PoolApi
mkPoolApi = do
    conn <- checkedConnect defaultConnectInfo
    _ <- print "Redis connection established..."
    pure $ PoolApi (putPredicted' conn) (putConfirmed' conn) (getLastPredicted' conn) (getLastConfirmed' conn) (existsPredicted' conn)

mkLastPredictedKey :: PoolId -> BSU.ByteString
mkLastPredictedKey (PoolId id) = BSU.fromString $ "last_predicted_" ++ (show id)

mkLastConfirmedKey :: PoolId -> BSU.ByteString
mkLastConfirmedKey (PoolId id) = BSU.fromString $ "last_confirmed_" ++ (show id)

mkPredictedNext :: PoolId -> TxId -> Integer -> BSU.ByteString
mkPredictedNext (PoolId id) (TxId txId) gix = BSU.fromString $ "predicted_next_" ++ (show txId) ++ (show gix) ++ "_" ++ (show id)

putPredicted' :: Connection -> PredictedPool Pool -> IO ()
putPredicted' conn (PredictedPool pool) = do
    res <- runRedis conn $ do
        let pIdLast = poolId $ poolData pool
            predictedNext =  mkPredictedNext pIdLast (refId $ fullTxOut pool) (gIdx $ gId pool)
            predictedLast =  mkLastPredictedKey pIdLast
            encodedPool = (BS.toStrict . encode) pool
        Redis.set predictedNext encodedPool
        Redis.set predictedLast encodedPool
    _ <- print res
    pure ()

putConfirmed' :: Connection -> ConfirmedPool Pool -> IO ()
putConfirmed' conn (ConfirmedPool pool) = do
    res <- runRedis conn $ do
        let pIdConfirmed = poolId $ poolData pool
            confirmed = mkLastConfirmedKey pIdConfirmed
            encodedPool = (BS.toStrict . encode) pool
        Redis.set confirmed encodedPool
    _ <- print res
    pure ()

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
        Redis.exists $ mkPredictedNext pId txId gix
    pure (unsafeFromEither res)