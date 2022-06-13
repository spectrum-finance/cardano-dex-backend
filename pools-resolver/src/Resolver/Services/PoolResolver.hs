module Resolver.Services.PoolResolver
    ( PoolResolver(..)
    , mkPoolResolver
    ) where

import RIO

import System.Logging.Hlog

import Resolver.Repositories.PoolRepository
import ErgoDex.Amm.Pool
import Explorer.Types
import Core.Types

data PoolResolver f = PoolResolver
  { resolve :: PoolId -> f (Maybe ConfirmedPool)
  }

mkPoolResolver :: (Monad i, MonadIO f) => PoolRepository f -> MakeLogging i f ->  i (PoolResolver f)
mkPoolResolver p MakeLogging{..} = do
  logger <- forComponent "poolsResolver"
  pure $ PoolResolver $ resolve' p logger

resolve' :: MonadIO f => PoolRepository f -> Logging f -> PoolId -> f (Maybe ConfirmedPool)
resolve' p@PoolRepository{..} logging@Logging{..} poolId = do
  lastConfirmed <- getLastConfirmed poolId
  _             <- infoM $ "lastConfirmed: " <> show lastConfirmed
  lastPredicted <- getLastPredicted poolId
  _             <- infoM $ "lastPredicted: " <> show lastPredicted
  process p logging lastConfirmed lastPredicted

process
  :: MonadIO f
  => PoolRepository f
  -> Logging f
  -> Maybe ConfirmedPool
  -> Maybe PredictedPool
  -> f (Maybe ConfirmedPool)
process p@PoolRepository{..} Logging{..} confirmedMaybe predictedMaybe = do
  case (confirmedMaybe, predictedMaybe) of
    (Just confirmed@(ConfirmedPool confEvent), Just predicted@(PredictedPool predEvent)) -> do
      consistentChain <- existsPredicted $ getPoolId confEvent
      pessimisticPool <- pessimistic consistentChain p predicted confirmed
      let
        upToDate = unGix (lastConfirmedOutGix confEvent) <= unGix (lastConfirmedOutGix predEvent) -- add Ord to Gix
        toReturn = if upToDate then ConfirmedPool predEvent else pessimisticPool
      pure $ Just toReturn
    (Just confirmed, _) -> infoM @String "Just only confirmed. Predicted is empty." $> Just confirmed
    _                   -> infoM @String "Both are nothing." $> Nothing

pessimistic
  :: MonadIO f
  => Bool
  -> PoolRepository f
  -> PredictedPool
  -> ConfirmedPool
  -> f ConfirmedPool
pessimistic consistentChain p predictedPool confirmedPool@(ConfirmedPool confEvent) =
  if consistentChain
  then needToUpdate p predictedPool (lastConfirmedOutGix confEvent)
  else pure confirmedPool

needToUpdate :: MonadIO f => PoolRepository f -> PredictedPool -> Gix -> f ConfirmedPool
needToUpdate PoolRepository{..} (PredictedPool predEvent) newGix = do
  let
    event       = predEvent {lastConfirmedOutGix = newGix}
    updatedPool = PredictedPool event
  _ <- putPredicted updatedPool
  pure $ ConfirmedPool event
