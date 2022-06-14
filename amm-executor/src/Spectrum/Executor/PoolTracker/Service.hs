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

import ErgoDex.Amm.Pool
  ( PoolId )
import Spectrum.Executor.Data.PoolState
  ( Pool, Predicted (Predicted), Confirmed (Confirmed), Unconfirmed (Unconfirmed) )
import Spectrum.Executor.PoolTracker.Data.Traced
  ( Traced (Traced) )
import Spectrum.Executor.PoolTracker.Persistence.Pools
  ( Pools(..) )
import Spectrum.Executor.Types
  ( PoolStateId )
import CardanoTx.Models
  ( FullTxOut(FullTxOut, fullTxOutRef) )
import ErgoDex.State
  ( OnChain(OnChain) )
import qualified ErgoDex.Amm.Pool as Core

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
    (Just (Confirmed confirmedPool), _, Just (Predicted predictedPool@(OnChain predictedOut _))) -> do
      let
        anchoringPool@(OnChain anchoringPoint _) = 
          fromMaybe confirmedPool (unconfirmedM <&> (\(Unconfirmed pool) -> pool))
        predictionIsAnchoringPoint = fullTxOutRef anchoringPoint == fullTxOutRef predictedOut
      predictionIsValid <-
        if predictionIsAnchoringPoint
          then pure True
          else trace pools (fullTxOutRef predictedOut) (fullTxOutRef anchoringPoint)
      pure . Just $ if predictionIsValid then predictedPool else anchoringPool
    (_, Just (Unconfirmed unconfirmedPool), Nothing) -> pure $ Just unconfirmedPool
    (Just (Confirmed confirmedPool), _, _) -> pure $ Just confirmedPool
    _ -> pure Nothing

trace :: Monad m => Pools m -> PoolStateId -> PoolStateId -> m Bool
trace pools@Pools{..} sid anchoringPoint =
  getPrediction sid >>= (\case
    Just (Traced (Predicted (OnChain _ _)) prevTxOutRef) | prevTxOutRef == anchoringPoint -> pure True
    Just (Traced (Predicted (OnChain _ _)) prevTxOutRef) -> trace pools prevTxOutRef anchoringPoint
    Nothing -> pure False)

invalidatePool'
  :: Pools m
  -> Pool
  -> m ()
invalidatePool' Pools{..} (OnChain FullTxOut{..} Core.Pool{..}) =
  invalidate poolId fullTxOutRef

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