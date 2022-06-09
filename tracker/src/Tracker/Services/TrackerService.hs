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
import GHC.Natural          as Natural (naturalToInt)
import Control.Monad.Catch
import RIO
import Data.Text.Internal.Fusion.Size (isEmpty)

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
  Items{..}  <- getUnspentOutputsRetry logging minIndex (Limit $ toInteger $ Natural.naturalToInt limitOffset) explorer
  let
    newMinIndex = if null items then unGix minIndex else (unGix. globalIndex . last $ items) + 1
  _        <- infoM $ "Going to put new Min index is " ++ show newMinIndex
  _        <- putMinIndex $ Gix newMinIndex
  pure items

getUnspentOutputsRetry
  :: (MonadIO f, MonadMask f)
  => Logging f
  -> Gix
  -> Limit
  -> Explorer f
  -> f (Items FullTxOut)
getUnspentOutputsRetry Logging{..} minIndex limit Explorer{..} = do
  let limitedBackoff = constantDelay 1000000
  recoverAll limitedBackoff (\rs -> infoM ("RetryStatus for getUnspentOutputs " ++ show rs) >> getUnspentOutputs minIndex limit Asc)