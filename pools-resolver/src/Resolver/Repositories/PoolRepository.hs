{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Resolver.Repositories.PoolRepository
  ( PoolRepository(..)
  , mkPoolRepository
  ) where

import RIO
import Plutus.V1.Ledger.TxId
import Prelude                     (print)
import RIO.ByteString.Lazy         as BS
import Data.Aeson                  as Json
import Database.Redis              as Redis
import Data.ByteString.UTF8        as BSU
import RIO.ByteString.Lazy         as LBS
import Resolver.Models.AppSettings (RedisSettings(..))
import Resolver.Utils
import ErgoDex.Amm.Pool
import ErgoDex.State
import Cardano.Models
import Core.Types
import Plutus.V1.Ledger.Tx

data PoolRepository f = PoolRepository
  { putPredicted     :: PredictedPool  -> f ()
  , putConfirmed     :: ConfirmedPool  -> f ()
  , getLastPredicted :: PoolId         -> f (Maybe PredictedPool)
  , getLastConfirmed :: PoolId         -> f (Maybe ConfirmedPool)
  , existsPredicted  :: PoolId         -> f Bool
  }

mkPoolRepository :: (MonadIO f) => RedisSettings -> f (PoolRepository f)
mkPoolRepository redis = do
    conn <- liftIO (checkedConnect $ defaultConnectInfo { connectHost = getRedisHost redis } )
    -- _ <- print "Redis connection established..." // todo: log info
    pure $ PoolRepository (putPredicted' conn) (putConfirmed' conn) (getLastPredicted' conn) (getLastConfirmed' conn) (existsPredicted' conn)

putPredicted' :: (MonadIO f) => Connection -> PredictedPool -> f ()
putPredicted' conn r@(OnChainIndexedEntity{entity=Pool{..}, txOut=FullTxOut{..}, lastConfirmedOutGix=gix}) = do
    res <- liftIO $ runRedis conn $ do
        let predictedNext = mkPredicted poolId
            predictedLast = mkLastPredictedKey poolId
            encodedPool = (BS.toStrict . encode) r
        Redis.set predictedNext encodedPool
        Redis.set predictedLast encodedPool
    liftIO $ print res

putConfirmed' :: (MonadIO f) => Connection -> ConfirmedPool -> f ()
putConfirmed' conn r@(OnChainIndexedEntity{entity=Pool{..}, txOut=FullTxOut{..}, lastConfirmedOutGix=gix}) = do
  res <- liftIO $ runRedis conn $ do
      let confirmed = mkLastConfirmedKey poolId
          encodedPool = (BS.toStrict . encode) r
          t = 1
      Redis.set confirmed encodedPool
  liftIO $ print res

getLastPredicted' :: (MonadIO f) =>  Connection -> PoolId -> f (Maybe PredictedPool)
getLastPredicted' conn id = liftIO $ getFromRedis conn (mkLastPredictedKey id)

getLastConfirmed' :: (MonadIO f) => Connection -> PoolId -> f (Maybe ConfirmedPool)
getLastConfirmed' conn id = liftIO $ getFromRedis conn (mkLastConfirmedKey id)

getFromRedis :: (MonadIO f, FromJSON a) => Connection -> BSU.ByteString -> f (Maybe a)
getFromRedis conn key = do
  maybeRes <- liftIO $ runRedis conn (Redis.get key)
  let resParsed = (unsafeFromEither maybeRes) >>= (Json.decode . LBS.fromStrict)
  pure resParsed

existsPredicted' :: (MonadIO f) => Connection -> PoolId -> f Bool
existsPredicted' conn pId = do
  res <- liftIO $ runRedis conn (Redis.exists $ mkPredicted pId)
  pure $ unsafeFromEither res

mkLastPredictedKey :: PoolId -> BSU.ByteString
mkLastPredictedKey (PoolId poolId) = BSU.fromString $ "predicted:last:" ++ show poolId

mkLastConfirmedKey :: PoolId -> BSU.ByteString
mkLastConfirmedKey (PoolId poolId) = BSU.fromString $ "confirmed:last:" ++ show poolId

mkPredicted :: PoolId -> BSU.ByteString
mkPredicted (PoolId pid) = BSU.fromString $ "predicted:" ++ show pid