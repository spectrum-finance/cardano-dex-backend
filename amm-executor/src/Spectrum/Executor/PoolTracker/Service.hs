module Spectrum.Executor.PoolTracker.Service
  ( PoolResolver(..)
  , mkPoolResolver
  ) where

import RIO
  ( (<&>), fromMaybe )

import Control.Monad.IO.Class
  ( MonadIO )

import System.Logging.Hlog
  ( MakeLogging (MakeLogging, forComponent), Logging (Logging, infoM) )

import qualified ErgoDex.Amm.Pool as Core
import ErgoDex.Amm.Pool
  ( PoolId )
import Spectrum.Executor.Data.State
  ( Predicted (Predicted), Confirmed (Confirmed), Unconfirmed (Unconfirmed) )
import Spectrum.Executor.PoolTracker.Data.Traced
  ( Traced (Traced) )
import Spectrum.Executor.PoolTracker.Persistence.Pools
  ( Pools(..) )
import Spectrum.Executor.Types
  ( PoolStateId (PoolStateId), Pool, poolStateId )
import ErgoDex.State
  ( OnChain(OnChain) )

data PoolResolver m = PoolResolver
  { resolvePool    :: PoolId -> m (Maybe Pool)
  , putPool        :: Traced (Predicted Pool) -> m ()
  , invalidatePool :: Pool -> m ()
  }

mkPoolResolver
  :: forall f m. (MonadIO f, MonadIO m)
  => MakeLogging f m
  -> Pools m
  -> f (PoolResolver m)
mkPoolResolver MakeLogging{..} pools@Pools{..} = do
  logging <- forComponent "PoolResolver"
  pure $ attachLogging logging PoolResolver
    { resolvePool    = resolvePool' pools
    , putPool        = putPredicted
    , invalidatePool = invalidatePool' pools
    }

resolvePool'
  :: Monad m
  => Pools m
  -> PoolId
  -> m (Maybe Pool)
resolvePool' pools@Pools{..} pid = do
  confirmedM   <- getLastConfirmed pid
  unconfirmedM <- getLastUnconfirmed pid
  predictedM   <- getLastPredicted pid
  case (confirmedM, unconfirmedM, predictedM) of
    (Just (Confirmed confirmedPool), _, Just (Predicted predictedPool)) -> do
      let
        anchoringPoint =
          fromMaybe confirmedPool (unconfirmedM <&> (\(Unconfirmed pool) -> pool))
        predictionIsAnchoringPoint = poolStateId anchoringPoint == poolStateId predictedPool
      predictionIsValid <-
        if predictionIsAnchoringPoint
          then pure True
          else trace pools (poolStateId predictedPool) (poolStateId anchoringPoint)
      pure . Just $ if predictionIsValid then predictedPool else anchoringPoint
    (_, Just (Unconfirmed unconfirmedPool), Nothing) -> pure $ Just unconfirmedPool
    (Just (Confirmed confirmedPool), _, _) -> pure $ Just confirmedPool
    _ -> pure Nothing

trace :: Monad m => Pools m -> PoolStateId -> PoolStateId -> m Bool
trace pools@Pools{..} sid anchoringState =
  getPrediction sid >>= (\case
    Just (Traced (Predicted (OnChain _ _)) prevTxOutRef) | PoolStateId prevTxOutRef == anchoringState -> pure True
    Just (Traced (Predicted (OnChain _ _)) prevTxOutRef) -> trace pools (PoolStateId prevTxOutRef) anchoringState
    Nothing -> pure False)

invalidatePool'
  :: Pools m
  -> Pool
  -> m ()
invalidatePool' Pools{..} pool@(OnChain _ Core.Pool{poolId}) =
  invalidate poolId (poolStateId pool)

attachLogging :: Monad m => Logging m -> PoolResolver m -> PoolResolver m
attachLogging Logging{..} PoolResolver{..}=
  PoolResolver
    { resolvePool = \pid -> do
        infoM $ "resolvePool " <> show pid
        r <- resolvePool pid
        infoM $ "resolvePool " <> show pid <> " -> " <> show r
        pure r
    , putPool = \tracedPool -> do
        infoM $ "putPool " <> show tracedPool
        r <- putPool tracedPool
        infoM $ "putPool " <> show tracedPool <> " -> " <> show r
        pure r
    , invalidatePool = \pool -> do
        infoM $ "invalidatePool " <> show pool
        r <- invalidatePool pool
        infoM $ "invalidatePool " <> show pool <> " -> " <> show r
        pure r
    }