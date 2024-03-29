module Spectrum.Executor.PoolTracker.Service
  ( PoolResolver(..)
  , mkPoolResolver
  ) where

import RIO
  ( (<&>), fromMaybe, MonadReader )

import Control.Monad.IO.Class
  ( MonadIO )

import System.Logging.Hlog
  ( MakeLogging (MakeLogging, forComponent), Logging (Logging, infoM, debugM) )

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
  ( PoolStateId (PoolStateId), Pool, poolStateId, extractPoolId )
import ErgoDex.State
  ( OnChain(OnChain) )
import Spectrum.Prelude.Context (HasType, askContext)

data PoolResolver m = PoolResolver
  { resolvePool    :: PoolId -> m (Maybe Pool)
  , putPool        :: Traced (Predicted Pool) -> m ()
  , invalidatePool :: Pool -> m ()
  }

mkPoolResolver
  :: forall f m env.
    ( MonadIO f
    , MonadIO m
    , MonadReader env f
    , HasType (MakeLogging f m) env
    )
  => Pools m
  -> f (PoolResolver m)
mkPoolResolver pools@Pools{..} = do
  MakeLogging{..} <- askContext
  logging <- forComponent "Bots.PoolResolver"
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
    Just (Traced (Predicted _) prevTxOutRef) | PoolStateId prevTxOutRef == anchoringState -> pure True
    Just (Traced (Predicted _) prevTxOutRef) -> trace pools (PoolStateId prevTxOutRef) anchoringState
    Nothing -> pure False)

invalidatePool'
  :: Pools m
  -> Pool
  -> m ()
invalidatePool' Pools{..} pool =
  invalidate (extractPoolId pool) (poolStateId pool)

attachLogging :: Monad m => Logging m -> PoolResolver m -> PoolResolver m
attachLogging Logging{..} PoolResolver{..}=
  PoolResolver
    { resolvePool = \pid -> do
        debugM $ "resolvePool " <> show pid
        r <- resolvePool pid
        debugM $ "resolvePool " <> show pid <> " -> " <> show r
        pure r
    , putPool = \tracedPool -> do
        debugM $ "putPool " <> show tracedPool
        r <- putPool tracedPool
        debugM $ "putPool " <> show tracedPool <> " -> " <> show r
        pure r
    , invalidatePool = \pool -> do
        debugM $ "invalidatePool " <> show pool
        r <- invalidatePool pool
        debugM $ "invalidatePool " <> show pool <> " -> " <> show r
        pure r
    }