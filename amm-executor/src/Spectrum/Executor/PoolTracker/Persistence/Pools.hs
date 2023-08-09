module Spectrum.Executor.PoolTracker.Persistence.Pools
  ( Pools(..)
  , mkPools
  , mkNonPersistentPools
  ) where

import RIO
  ( IsString(fromString), ByteString, newIORef, readIORef, (<&>), atomicModifyIORef' )

import qualified Database.RocksDB as Rocks

import qualified Data.ByteString.UTF8 as Utf8
import Data.Aeson
  ( FromJSON )

import System.Logging.Hlog
  ( MakeLogging(..), Logging (Logging, infoM, debugM) )

import Control.Monad.IO.Class
  ( MonadIO )
import Control.Monad.Trans.Resource
  ( MonadResource )
import Control.Monad.Catch
  ( MonadThrow )
import Data.Map as Map

import qualified ErgoDex.Amm.Pool as Core
import ErgoDex.State
  ( OnChain(OnChain) )
import Spectrum.Executor.Types
  ( Pool, PoolStateId, poolStateId, PoolId (PoolId) )
import Spectrum.Executor.Data.State
  ( Predicted (Predicted), Confirmed (Confirmed), Unconfirmed (Unconfirmed) )
import Spectrum.Executor.PoolTracker.Data.Traced
  ( Traced (Traced) )
import Spectrum.Executor.PoolTracker.Persistence.Config
  ( PoolStoreConfig(..) )
import Spectrum.Common.Persistence.Serialization
  ( serialize, deserializeM )
import Spectrum.Prelude.Context
  ( MonadReader, HasType, askContext )
import GHC.IORef (IORef(IORef))

data Pools m = Pools
  { getPrediction      :: PoolStateId -> m (Maybe (Traced (Predicted Pool)))
  , getLastPredicted   :: PoolId -> m (Maybe (Predicted Pool))
  , getLastConfirmed   :: PoolId -> m (Maybe (Confirmed Pool))
  , getLastUnconfirmed :: PoolId -> m (Maybe (Unconfirmed Pool))
  , putPredicted       :: Traced (Predicted Pool) -> m ()
  , putConfirmed       :: Confirmed Pool -> m ()
  , putUnconfirmed     :: Unconfirmed Pool -> m ()
  , invalidate         :: PoolId -> PoolStateId -> m ()
  }

mkNonPersistentPools
  :: forall f m env.
    ( MonadIO f
    , MonadIO m
    , MonadThrow m
    , MonadReader env f
    , HasType (MakeLogging f m) env
    , HasType PoolStoreConfig env
    )
  => f (Pools m)
mkNonPersistentPools = do
  PoolStoreConfig{..} <- askContext
  MakeLogging{..}     <- askContext

  refMap <- newIORef empty :: f (IORef (Map.Map ByteString ByteString))

  logging <- forComponent "Bots.Pools"
  -- (_, db) <- Rocks.openBracket storePath
  --             Rocks.defaultOptions
  --               { Rocks.createIfMissing = createIfMissing
  --               }
  let
    -- get :: k -> m (Maybe a)
    get :: FromJSON a => ByteString -> m (Maybe a)
    get = \key -> readIORef refMap <&> Map.lookup key >>= (\elem -> deserializeM `traverse` elem)
    put = \key value -> atomicModifyIORef' refMap (\storageMap -> (Map.insert key value storageMap, ()))
    delete = \key -> atomicModifyIORef' refMap (\storageMap -> (Map.delete key storageMap, ()))
  pure $ attachLogging logging Pools
    { getPrediction      = get . mkPredictedKey
    , getLastPredicted   = get . mkLastPredictedKey
    , getLastConfirmed   = get . mkLastConfirmedKey
    , getLastUnconfirmed = get . mkLastUnconfirmedKey

    , putPredicted =
        \tpp@(Traced pp@(Predicted pool@(OnChain _ Core.Pool{poolId})) _) -> do
          put (mkPredictedKey $ poolStateId pool) (serialize tpp)
          put (mkLastPredictedKey poolId) (serialize pp)

    , putConfirmed =
        \cp@(Confirmed pool@(OnChain _ Core.Pool{poolId})) -> do
          currentLastConfirmed <- get @(Confirmed Pool) . mkLastConfirmedKey $ poolId
          put (mkPrevConfirmedKey $ poolStateId pool) (serialize currentLastConfirmed)
          put (mkLastConfirmedKey poolId) (serialize cp)

    , putUnconfirmed =
        \up@(Unconfirmed (OnChain _ Core.Pool{poolId})) ->
          put (mkLastUnconfirmedKey poolId) (serialize up)

    , invalidate = \pid sid -> do
        predM <- get @(Predicted Pool) $ mkLastPredictedKey pid
        mapM_
          (\((Predicted pool)) ->
            if poolStateId pool == sid
              then delete (mkPredictedKey sid) >> delete (mkLastPredictedKey pid)
              else pure () )
          predM
        unconfirmedM <- get @(Unconfirmed Pool) $ mkLastUnconfirmedKey pid
        mapM_
          (\((Unconfirmed pool)) ->
            if poolStateId pool == sid
              then delete $ mkLastUnconfirmedKey pid
              else pure () )
          unconfirmedM
        confirmedM <- get @(Confirmed Pool) $ mkLastConfirmedKey pid
        mapM_ (\(Confirmed pool) ->
          if poolStateId pool == sid
          then do
            prevConfPoolM <- get @(Confirmed Pool) $ mkPrevConfirmedKey sid
            case prevConfPoolM of
              Just prevConfirmedPool ->
                delete (mkPrevConfirmedKey sid) >>
                put (mkLastConfirmedKey pid) (serialize prevConfirmedPool)
              Nothing ->
                delete (mkLastConfirmedKey pid)
          else pure () ) confirmedM
    }

mkPools
  :: forall f m env.
    ( MonadIO f
    , MonadResource f
    , MonadIO m
    , MonadThrow m
    , MonadReader env f
    , HasType (MakeLogging f m) env
    , HasType PoolStoreConfig env
    )
  => f (Pools m)
mkPools = do
  PoolStoreConfig{..} <- askContext
  MakeLogging{..}     <- askContext

  logging <- forComponent "Bots.Pools"
  (_, db) <- Rocks.openBracket storePath
              Rocks.defaultOptions
                { Rocks.createIfMissing = createIfMissing
                }
  let
    get :: FromJSON a => ByteString -> m (Maybe a)
    get = (=<<) (mapM deserializeM) . Rocks.get db Rocks.defaultReadOptions
    put = Rocks.put db Rocks.defaultWriteOptions
    delete = Rocks.delete db Rocks.defaultWriteOptions
  pure $ attachLogging logging Pools
    { getPrediction      = get . mkPredictedKey
    , getLastPredicted   = get . mkLastPredictedKey
    , getLastConfirmed   = get . mkLastConfirmedKey
    , getLastUnconfirmed = get . mkLastUnconfirmedKey

    , putPredicted =
        \tpp@(Traced pp@(Predicted pool@(OnChain _ Core.Pool{poolId})) _) -> do
          put (mkPredictedKey $ poolStateId pool) (serialize tpp)
          put (mkLastPredictedKey poolId) (serialize pp)

    , putConfirmed =
        \cp@(Confirmed pool@(OnChain _ Core.Pool{poolId})) -> do
          currentLastConfirmed <- get @(Confirmed Pool) . mkLastConfirmedKey $ poolId
          put (mkPrevConfirmedKey $ poolStateId pool) (serialize currentLastConfirmed)
          put (mkLastConfirmedKey poolId) (serialize cp)

    , putUnconfirmed =
        \up@(Unconfirmed (OnChain _ Core.Pool{poolId})) ->
          put (mkLastUnconfirmedKey poolId) (serialize up)

    , invalidate = \pid sid -> do
        predM <- get @(Predicted Pool) $ mkLastPredictedKey pid
        mapM_
          (\((Predicted pool)) ->
            if poolStateId pool == sid
              then delete (mkPredictedKey sid) >> delete (mkLastPredictedKey pid)
              else pure () )
          predM
        unconfirmedM <- get @(Unconfirmed Pool) $ mkLastUnconfirmedKey pid
        mapM_
          (\((Unconfirmed pool)) ->
            if poolStateId pool == sid
              then delete $ mkLastUnconfirmedKey pid
              else pure () )
          unconfirmedM
        confirmedM <- get @(Confirmed Pool) $ mkLastConfirmedKey pid
        mapM_ (\(Confirmed pool) ->
          if poolStateId pool == sid
          then do
            prevConfPoolM <- get @(Confirmed Pool) $ mkPrevConfirmedKey sid
            case prevConfPoolM of
              Just prevConfirmedPool ->
                delete (mkPrevConfirmedKey sid) >>
                put (mkLastConfirmedKey pid) (serialize prevConfirmedPool)
              Nothing ->
                delete (mkLastConfirmedKey pid)
          else pure () ) confirmedM
    }

attachLogging :: Monad m => Logging m -> Pools m -> Pools m
attachLogging Logging{..} Pools{..} =
  Pools
    { getPrediction = \pid -> do
        debugM $ "getPrediction " <> show pid
        r <- getPrediction pid
        debugM $ "getPrediction " <> show pid <> " -> " <> show r
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
    , getLastUnconfirmed = \pid -> do
        debugM $ "getLastUnconfirmed " <> show pid
        r <- getLastUnconfirmed pid
        debugM $ "getLastUnconfirmed " <> show pid <> " -> " <> show r
        pure r
    , putPredicted = \pp -> do
        debugM $ "putPredicted " <> show pp
        r <- putPredicted pp
        debugM $ "putPredicted " <> show pp <> " -> " <> show r
        pure r
    , putConfirmed = \pp -> do
        debugM $ "putConfirmed " <> show pp
        r <- putConfirmed pp
        debugM $ "putConfirmed " <> show pp <> " -> " <> show r
        pure r
    , putUnconfirmed = \pp -> do
        debugM $ "putUnconfirmed " <> show pp
        r <- putUnconfirmed pp
        debugM $ "putUnconfirmed " <> show pp <> " -> " <> show r
        pure r
    , invalidate = \pid sid -> do
        debugM $ "invalidate pid: " <> show pid <> ". sid: " <> show sid
        r <- invalidate pid sid
        debugM $ "invalidate pid: " <> show pid <> ", sid: " <> show sid <> " -> " <> show r
        pure r
    }

mkLastPredictedKey :: PoolId -> ByteString
mkLastPredictedKey (PoolId pid) = Utf8.fromString $ "predicted:last:" <> show pid

mkLastConfirmedKey :: PoolId -> ByteString
mkLastConfirmedKey (PoolId pid) = Utf8.fromString $ "confirmed:last:" <> show pid

mkLastUnconfirmedKey :: PoolId -> ByteString
mkLastUnconfirmedKey (PoolId pid) = Utf8.fromString $ "unconfirmed:last:" <> show pid

mkPredictedKey :: PoolStateId -> ByteString
mkPredictedKey sid = fromString $ "predicted:prev:" <> show sid

mkPrevConfirmedKey :: PoolStateId -> ByteString
mkPrevConfirmedKey sid = Utf8.fromString $ "confirmed:prev:" <> show sid
