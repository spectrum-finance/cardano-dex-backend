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
import Control.Retry
import GHC.Natural          as Natural ( naturalToInt, naturalToInteger)
import Control.Monad.Catch
import RIO

data TrackerService f = TrackerService
 { getOutputs :: f [FullTxOut]
 }

mkTrackerService
  :: (Monad i, MonadIO f, MonadMask f)
  => TrackerServiceConfig
  -> MakeLogging i f
  -> TrackerCache f
  -> Explorer f
  -> i (TrackerService f)
mkTrackerService settings MakeLogging{..} cache client = do
  logger <- forComponent "trackerService"
  pure $ TrackerService $ getOutputs' settings logger cache client

getOutputs'
  :: (MonadIO f, MonadMask f)
  => TrackerServiceConfig
  -> Logging f
  -> TrackerCache f
  -> Explorer f
  -> f [FullTxOut]
getOutputs' TrackerServiceConfig{..} logging@Logging{..} TrackerCache{..} explorer = do
  _        <- infoM @String "Going to fetch min index"
  minIndex <- getMinIndex
  _        <- infoM $ "Min index is " ++ show minIndex
  let maxAttemptsInt = Natural.naturalToInt maxAttempts
  Items{..}  <- getUnspentOutputsRetry maxAttemptsInt logging minIndex (Limit $ toInteger $ Natural.naturalToInt limitOffset) explorer
  let 
    lastOutputGix = globalIndex . last $ items
    newMinIndex   = unGix minIndex + unGix lastOutputGix + 1
  _        <- infoM $ "Going to put new Min index is " ++ show newMinIndex
  _        <- putMinIndex $ Gix newMinIndex
  pure items


getUnspentOutputsRetry
  :: (MonadIO f, MonadMask f)
  => Int
  -> Logging f
  -> Gix
  -> Limit
  -> Explorer f
  -> f (Items FullTxOut)
getUnspentOutputsRetry maxAttempts Logging{..} minIndex limit Explorer{..} = do
  let limitedBackoff = exponentialBackoff 1000000 <> limitRetries maxAttempts
  recoverAll limitedBackoff (\rs -> infoM ("RetryStatus for getUnspentOutputs " ++ show rs) >> getUnspentOutputs minIndex limit)