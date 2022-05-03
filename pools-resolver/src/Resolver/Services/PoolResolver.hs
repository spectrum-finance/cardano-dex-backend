module Resolver.Services.PoolResolver
    ( PoolResolver(..)
    , mkPoolResolver
    ) where

import RIO
import Resolver.Repositories.PoolRepository
import Prelude                               (print)
import ErgoDex.Amm.Pool
import ErgoDex.State
import CardanoTx.Models
import Explorer.Types
import Plutus.V1.Ledger.Tx
import Core.Types
import System.Logging.Hlog

data PoolResolver f = PoolResolver
  { resolve :: PoolId -> f (Maybe ConfirmedPool)
  }

mkPoolResolver :: (Monad i, MonadIO f) => PoolRepository f -> MakeLogging i f ->  i (PoolResolver f)
mkPoolResolver p MakeLogging{..} = do
  logger <- forComponent "poolsResolver"
  pure $ PoolResolver $ resolve' p logger

resolve' :: (MonadIO f) => PoolRepository f -> Logging f -> PoolId -> f (Maybe ConfirmedPool)
resolve' p@PoolRepository{..} logging@Logging{..} poolId = do
  lastConfirmed <- getLastConfirmed poolId
  _             <- infoM ("lastConfirmed: " ++ (show lastConfirmed) ++ ". PoolId: " ++ (show poolId))
  lastPredicted <- getLastPredicted poolId
  _             <- infoM ("lastPredicted: " ++ (show lastPredicted) ++ ". PoolId: " ++ (show poolId))
  process p logging lastConfirmed lastPredicted

process
  :: (MonadIO f)
  => PoolRepository f
  -> Logging f
  -> Maybe ConfirmedPool
  -> Maybe PredictedPool
  -> f (Maybe ConfirmedPool)
process p@PoolRepository{..} logging@Logging{..} confirmedMaybe predictedMaybe = do
  case (confirmedMaybe, predictedMaybe) of
    (Just confirmed@(ConfirmedPool confEvent), Just predicted@(PredictedPool predEvent)) -> do
      consistentChain  <- existsPredicted $ getPoolId confEvent
      _ <- infoM @String ("consistentChain: " ++ (show consistentChain))
      pessimisticPool  <- pessimistic consistentChain logging p predicted confirmed
      _ <- infoM @String ("pessimisticPool: " ++ (show pessimisticPool))
      let
        upToDate  = unGix (lastConfirmedOutGix confEvent) <= unGix (lastConfirmedOutGix predEvent) -- add Ord to Gix
        toReturn  = if upToDate then (ConfirmedPool predEvent) else pessimisticPool
      _ <- infoM @String ("upToDate: " ++ (show upToDate))
      _ <- infoM @String ("toReturn: " ++ (show toReturn))
      pure $ Just toReturn
    (Just confirmed, _) -> do
      _ <- infoM @String ("Just only confirmed. Predicted is empty. Confirmed: " ++ (show confirmed))
      pure $ Just confirmed
    _ -> do
      _ <- infoM @String ("Both are nothing.")
      pure Nothing

pessimistic
  :: (MonadIO f)
  => Bool
  -> Logging f
  -> PoolRepository f
  -> PredictedPool
  -> ConfirmedPool
  -> f ConfirmedPool
pessimistic consistentChain logging p predictedPool confirmedPool@(ConfirmedPool confEvent) =
  if consistentChain
  then needToUpdate p logging predictedPool (lastConfirmedOutGix confEvent)
  else pure $ confirmedPool

needToUpdate :: (MonadIO f) => PoolRepository f -> Logging f ->  PredictedPool -> Gix -> f ConfirmedPool
needToUpdate PoolRepository{..} Logging{..} predicted@(PredictedPool predEvent) newGix = do
  let
    event       = predEvent {lastConfirmedOutGix = newGix}
    updatedPool = PredictedPool event
  _   <- infoM @String ("updatedPool: " ++ (show updatedPool))
  res <- putPredicted updatedPool
  _   <- infoM @String ("needToUpdate pool. result: " ++ (show res))
  pure $ ConfirmedPool event

