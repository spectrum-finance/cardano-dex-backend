module Tracker.Services.TrackerService
  ( TrackerService(..)
  , mkTrackerService
  ) where

import Tracker.Models.AppConfig
import Tracker.Caches.TrackerCache
import Tracker.Services.Logger as Log

import Explorer.Models
import Explorer.Service
import Explorer.Types

import Prelude
import GHC.Natural as Natural ( naturalToInt, naturalToInteger)
import RIO

data TrackerService f = TrackerService
 { getOutputs :: f [FullTxOut]
 }

mkTrackerService
  :: MonadIO f
  => TrackerServiceConfig
  -> TrackerCache f
  -> Explorer f
  -> TrackerService f
mkTrackerService settings cache client = TrackerService $ getOutputs' settings cache client

getOutputs'
  :: (Monad f, MonadIO f)
  => TrackerServiceConfig
  -> TrackerCache f
  -> Explorer f
  -> f [FullTxOut]
getOutputs' TrackerServiceConfig{..} TrackerCache{..} Explorer{..} = do
  _        <- Log.log "Going to fetch min index"
  minIndex <- getMinIndex
  _        <- Log.log $ "Min index is " ++ show minIndex
  outputs  <- getUnspentOutputs minIndex (Limit $ toInteger $ Natural.naturalToInt limitOffset)
  let newMinIndex = unGix minIndex + toInteger (length $ items outputs)
  _        <- Log.log $ "Going to put new Min index is " ++ show newMinIndex
  _        <- putMinIndex $ Gix $ newMinIndex
  pure $ items outputs
