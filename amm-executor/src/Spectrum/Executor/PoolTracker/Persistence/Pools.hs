module Spectrum.Executor.PoolTracker.Persistence.Pools
  ( Pools(..)
  , mkPools
  ) where

import RIO
  ( IsString(fromString), ByteString )

import qualified Database.RocksDB as Rocks

import qualified Data.ByteString.UTF8 as Utf8

import System.Logging.Hlog
  ( MakeLogging(..), Logging (Logging, infoM) )

import Control.Monad.IO.Class
  ( MonadIO (liftIO) )
import Control.Monad.Trans.Resource
  ( MonadResource )

import ErgoDex.Amm.Pool
  ( PoolId (PoolId) )
import ErgoDex.State
  ( OnChain (OnChain) )
import Spectrum.Executor.Data.PoolState
  ( Predicted (Predicted), Confirmed (Confirmed), Unconfirmed (Unconfirmed), Pool )
import Spectrum.Executor.PoolTracker.Data.Traced
  ( Traced (Traced) )
import Spectrum.Executor.PoolTracker.Persistence.Config
  ( PoolStoreConfig(..) )
import Spectrum.Common.Persistence.Serialization
  ( serialize, deserializeM )
import qualified ErgoDex.Amm.Pool as Core

data Pools m = Pools
  { getPrediction      :: PoolId -> m (Maybe (Traced (Predicted Pool)))
  , getLastPredicted   :: PoolId -> m (Maybe (Predicted Pool))
  , getLastConfirmed   :: PoolId -> m (Maybe (Confirmed Pool))
  , getLastUnconfirmed :: PoolId -> m (Maybe (Unconfirmed Pool))
  , putPredicted       :: Traced (Predicted Pool) -> m ()
  , putConfirmed       :: Confirmed Pool -> m ()
  , putUnconfirmed     :: Unconfirmed Pool -> m ()
  }

mkPools
  :: (MonadIO f, MonadResource f, MonadIO m)
  => MakeLogging f m
  -> PoolStoreConfig
  -> f (Pools m)
mkPools MakeLogging{..} PoolStoreConfig{..} = do
  logging <- forComponent "Pools"
  db      <- Rocks.open storePath
              Rocks.defaultOptions
                { Rocks.createIfMissing = createIfMissing
                }
  let
    readopts  = Rocks.defaultReadOptions
    writeopts = Rocks.defaultWriteOptions
  pure $ attachLogging logging Pools
    { getPrediction =
        \pid -> liftIO $ Rocks.get db readopts (mkPredictedKey pid) >>= mapM deserializeM
    , getLastPredicted =
        \pid -> liftIO $ Rocks.get db readopts (mkLastPredictedKey pid) >>= mapM deserializeM
    , getLastConfirmed =
        \pid -> liftIO $ Rocks.get db readopts (mkLastConfirmedKey pid) >>= mapM deserializeM
    , getLastUnconfirmed =
        \pid -> liftIO $ Rocks.get db readopts (mkLastUnconfirmedKey pid) >>= mapM deserializeM
    , putPredicted =
        \pp@(Traced (Predicted (OnChain _ Core.Pool{..})) _) ->
          Rocks.put db writeopts (mkPredictedKey poolId) (serialize pp)
    , putConfirmed =
        \cp@(Confirmed (OnChain _ Core.Pool{..})) ->
          Rocks.put db writeopts (mkPredictedKey poolId) (serialize cp)
    , putUnconfirmed =
        \up@(Unconfirmed (OnChain _ Core.Pool{..})) ->
          Rocks.put db writeopts (mkPredictedKey poolId) (serialize up)
    }

attachLogging :: Monad m => Logging m -> Pools m -> Pools m
attachLogging Logging{..} Pools{..} =
  Pools
    { getPrediction = \pid -> do
        infoM $ "getPrediction " <> show pid
        r <- getPrediction pid
        infoM $ "getPrediction " <> show pid <> " -> " <> show r
        pure r
    , getLastPredicted = \pid -> do
        infoM $ "getLastPredicted " <> show pid
        r <- getLastPredicted pid
        infoM $ "getLastPredicted " <> show pid <> " -> " <> show r
        pure r
    , getLastConfirmed = \pid -> do
        infoM $ "getLastConfirmed " <> show pid
        r <- getLastConfirmed pid
        infoM $ "getLastConfirmed " <> show pid <> " -> " <> show r
        pure r
    , getLastUnconfirmed = \pid -> do
        infoM $ "getLastUnconfirmed " <> show pid
        r <- getLastUnconfirmed pid
        infoM $ "getLastUnconfirmed " <> show pid <> " -> " <> show r
        pure r
    , putPredicted = \pp -> do
        infoM $ "putPredicted " <> show pp
        r <- putPredicted pp
        infoM $ "putPredicted " <> show pp <> " -> " <> show r
        pure r
    , putConfirmed = \pp -> do
        infoM $ "putConfirmed " <> show pp
        r <- putConfirmed pp
        infoM $ "putConfirmed " <> show pp <> " -> " <> show r
        pure r
    , putUnconfirmed = \pp -> do
        infoM $ "putUnconfirmed " <> show pp
        r <- putUnconfirmed pp
        infoM $ "putUnconfirmed " <> show pp <> " -> " <> show r
        pure r
    }

mkLastPredictedKey :: PoolId -> ByteString
mkLastPredictedKey (PoolId poolId) = Utf8.fromString $ "predicted:last:" <> show poolId

mkLastConfirmedKey :: PoolId -> ByteString
mkLastConfirmedKey (PoolId poolId) = Utf8.fromString $ "confirmed:last:" <> show poolId

mkLastUnconfirmedKey :: PoolId -> ByteString
mkLastUnconfirmedKey (PoolId poolId) = Utf8.fromString $ "unconfirmed:last:" <> show poolId

mkPredictedKey :: PoolId -> ByteString
mkPredictedKey (PoolId pid) = fromString $ "predicted:prev:" <> show pid
