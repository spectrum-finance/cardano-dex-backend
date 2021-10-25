module Resolver.Services.PoolResolver
    ( PoolResolver(..)
    , mkPoolResolver
    ) where

import RIO
import Resolver.Repositories.PoolRepository
import Prelude                               (print)
import ErgoDex.Amm.Pool
import ErgoDex.State
import Cardano.Models
import Explorer.Types
import Plutus.V1.Ledger.Tx
import Resolver.Models.Types

data PoolResolver f = PoolResolver
  { resolve :: PoolId -> f (Maybe ConfirmedPool)
  }

mkPoolResolver :: (MonadIO f) => PoolRepository f -> PoolResolver f
mkPoolResolver p = PoolResolver $ resolve' p

resolve' :: (MonadIO f) => PoolRepository f -> PoolId -> f (Maybe ConfirmedPool)
resolve' p@PoolRepository{..} poolId = do
  lastConfirmed <- getLastConfirmed poolId
  _             <- liftIO $ print lastConfirmed -- todo: add log.info
  lastPredicted <- getLastPredicted poolId
  _             <- liftIO $ print lastPredicted -- todo: add log.info
  process p lastConfirmed lastPredicted

process
  :: (MonadIO f)
  => PoolRepository f
  -> Maybe ConfirmedPool
  -> Maybe PredictedPool
  -> f (Maybe ConfirmedPool)
process p@PoolRepository{..} confirmedMaybe predictedMaybe = do
  case (confirmedMaybe, predictedMaybe) of
    (Just confirmed, Just predicted) -> do
      consistentChain  <- existsPredicted $ getPoolId confirmed
      pessimisticPool  <- pessimistic consistentChain p confirmed predicted
      let
        upToDate  = unGix (lastConfirmedOutGix confirmed) <= unGix (lastConfirmedOutGix predicted) -- add Ord to Gix
        toReturn  = if upToDate then predicted else pessimisticPool
      pure $ Just toReturn
    (Just confirmed, _) -> do
      _ <- liftIO $ print "Just only confirmed. Predicted is empty." -- log.info here
      pure $ Just confirmed
    _ -> do
      _ <- liftIO $ print "Both are nothing." -- log.info here
      pure Nothing

pessimistic
  :: (MonadIO f)
  => Bool
  -> PoolRepository f
  -> PredictedPool
  -> ConfirmedPool
  -> f ConfirmedPool
pessimistic consistentChain p predictedPool confirmedPool =
  if consistentChain
  then needToUpdate p predictedPool (lastConfirmedOutGix confirmedPool)
  else pure $ confirmedPool

needToUpdate :: (MonadIO f) => PoolRepository f -> PredictedPool -> Gix -> f PredictedPool
needToUpdate PoolRepository{..} predicted newGix = do
  let updatedPool = predicted {lastConfirmedOutGix = newGix}
  _ <- putPredicted updatedPool
  pure predicted

