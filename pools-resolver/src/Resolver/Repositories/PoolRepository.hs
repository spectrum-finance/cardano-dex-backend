{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Resolver.Repositories.PoolRepository
  ( PoolRepository(..)
  , mkPoolRepository
  ) where

import RIO

import Control.Monad.Trans.Resource (MonadResource)

import System.Logging.Hlog (Logging(Logging, debugM), MakeLogging(..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as UBS
import           Data.Aeson           (ToJSON, decode, encode)

import Resolver.Settings (PoolStoreSettings(..))
import ErgoDex.Amm.Pool
import Core.Types

import qualified Database.RocksDB as Rocks

data PoolRepository f = PoolRepository
  { putPredicted     :: PredictedPool  -> f ()
  , putConfirmed     :: ConfirmedPool  -> f ()
  , getLastPredicted :: PoolId         -> f (Maybe PredictedPool)
  , getLastConfirmed :: PoolId         -> f (Maybe ConfirmedPool)
  , existsPredicted  :: PoolId         -> f Bool
  }

mkPoolRepository
  :: (MonadIO f, MonadResource f, MonadIO m)
  => PoolStoreSettings
  -> MakeLogging f m
  -> f (PoolRepository m)
mkPoolRepository PoolStoreSettings{..} MakeLogging{..} = do
  logging <- forComponent "PoolRepository"
  db      <- Rocks.open storePath
              Rocks.defaultOptions
                { Rocks.createIfMissing = createIfMissing
                }
  pure $ attachTracing logging PoolRepository
    { putPredicted     = putPredicted' db Rocks.defaultWriteOptions
    , putConfirmed     = putConfirmed' db Rocks.defaultWriteOptions 
    , getLastPredicted = getLastPredicted' db Rocks.defaultReadOptions
    , getLastConfirmed = getLastConfirmed' db Rocks.defaultReadOptions
    , existsPredicted  = existsPredicted' db Rocks.defaultReadOptions
    }

putPredicted' :: MonadIO f => Rocks.DB -> Rocks.WriteOptions -> PredictedPool -> f ()
putPredicted' db opts r@(PredictedPool OnChainIndexedEntity{entity=Pool{..}}) = do
  let predictedNext = mkPredictedKey poolId
      predictedLast = mkLastPredictedKey poolId
      encodedPool   = encodeStrict r
  Rocks.put db opts predictedNext encodedPool
  Rocks.put db opts predictedLast encodedPool

putConfirmed' :: MonadIO f => Rocks.DB -> Rocks.WriteOptions -> ConfirmedPool -> f ()
putConfirmed' db opts r@(ConfirmedPool OnChainIndexedEntity{entity=Pool{..}}) =
  liftIO $ Rocks.put db opts (mkLastConfirmedKey poolId) (encodeStrict r)

getLastPredicted' :: MonadIO f => Rocks.DB -> Rocks.ReadOptions -> PoolId -> f (Maybe PredictedPool)
getLastPredicted' db opts pid = liftIO $ Rocks.get db opts (mkLastPredictedKey pid) <&> (>>= (decode . LBS.fromStrict))

getLastConfirmed' :: MonadIO f => Rocks.DB -> Rocks.ReadOptions -> PoolId -> f (Maybe ConfirmedPool)
getLastConfirmed' db opts pid = liftIO $ Rocks.get db opts (mkLastConfirmedKey pid) <&> (>>= (decode . LBS.fromStrict))

existsPredicted' :: MonadIO f => Rocks.DB -> Rocks.ReadOptions -> PoolId -> f Bool
existsPredicted' db opts pid = liftIO $ Rocks.get db opts (mkPredictedKey pid) <&> isJust

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = LBS.toStrict . encode

mkLastPredictedKey :: PoolId -> ByteString
mkLastPredictedKey (PoolId poolId) = UBS.fromString $ "predicted:last:" ++ show poolId

mkLastConfirmedKey :: PoolId -> ByteString
mkLastConfirmedKey (PoolId poolId) = UBS.fromString $ "confirmed:last:" ++ show poolId

mkPredictedKey :: PoolId -> ByteString
mkPredictedKey (PoolId pid) = fromString $ "predicted:prev:" ++ show pid

attachTracing :: Monad m => Logging m -> PoolRepository m -> PoolRepository m
attachTracing Logging{..} PoolRepository{..} =
  PoolRepository
    { putPredicted = \pool -> do
        debugM $ "putPredicted " <> show pool
        r <- putPredicted pool
        debugM $ "putPredicted " <> show pool <> " -> " <> show r
        pure r
    , putConfirmed = \pool -> do
        debugM $ "putConfirmed " <> show pool
        r <- putConfirmed pool
        debugM $ "putConfirmed " <> show pool <> " -> " <> show r
        pure r
    , getLastPredicted = \pid -> do
        debugM $ "getLastPredicted " <> show pid
        r <- getLastPredicted pid
        debugM $ "getLastPredicted " <> show pid <> " -> " <> show r
        pure r
    , getLastConfirmed = \pid -> do
        debugM $ "getLastConfirmed " <> show pid
        r <- getLastConfirmed pid
        debugM $ "getLastConfirmed " <> show pid <> " -> " <> show r
        pure r
    , existsPredicted = \pid -> do
        debugM $ "existsPredicted " <> show pid
        r <- existsPredicted pid
        debugM $ "existsPredicted " <> show pid <> " -> " <> show r
        pure r
    }
