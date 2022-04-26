module Tracker.Services.TrackerService
  ( TrackerService(..)
  , mkTrackerService
  ) where

import Tracker.Models.AppConfig
import Tracker.Caches.TrackerCache

import Explorer.Models
import Explorer.Service
import Explorer.Types

import Prelude
import System.Logging.Hlog
import GHC.Natural          as Natural ( naturalToInt, naturalToInteger)
import RIO
import           ErgoDex.Contracts.Pool  (maxLqCapAmount)

data TrackerService f = TrackerService
 { getOutputs :: f [FullTxOut]
 }

mkTrackerService
  :: (Monad i, MonadIO f)
  => TrackerServiceConfig
  -> MakeLogging i f
  -> TrackerCache f
  -> Explorer f
  -> i (TrackerService f)
mkTrackerService settings MakeLogging{..} cache client = do
  logger <- forComponent "trackerService"
  pure $ TrackerService $ getOutputs' settings logger cache client

getOutputs'
  :: (Monad f, MonadIO f)
  => TrackerServiceConfig
  -> Logging f
  -> TrackerCache f
  -> Explorer f
  -> f [FullTxOut]
getOutputs' TrackerServiceConfig{..} Logging{..} TrackerCache{..} Explorer{..} = do
  _        <- infoM @String ("Going to fetch min index")
  minIndex <- getMinIndex
  _        <- infoM $ "Min index is " ++ show minIndex
  outputs  <- getUnspentOutputs minIndex (Limit $ toInteger $ Natural.naturalToInt limitOffset)
  let newMinIndex = unGix minIndex + toInteger (length $ items outputs)
  _        <- infoM $ "Going to put new Min index is " ++ show newMinIndex
  _        <- putMinIndex $ Gix $ newMinIndex
  pure $ items outputs
