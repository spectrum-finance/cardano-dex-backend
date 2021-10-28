module Tracker.Services.TrackerService 
  ( TrackerService(..)
  , mkTrackerService
  ) where 

import Tracker.Models.AppConfig
import Tracker.Repository.ExplorerRepo

import Explorer.Models
import Explorer.Service
import Explorer.Types

import Prelude
import GHC.Natural as Natural

data TrackerService f = TrackerService
 { getOutputs :: f [FullTxOut] 
 }

mkTrackerService 
  :: Monad f 
  => TrackerServiceConfig 
  -> ExplorerRepo f 
  -> Explorer f 
  -> TrackerService f
mkTrackerService settings repo client = TrackerService $ getOutputs' settings repo client

getOutputs'
  :: Monad f 
  => TrackerServiceConfig
  -> ExplorerRepo f 
  -> Explorer f 
  -> f [FullTxOut]
getOutputs' TrackerServiceConfig{..} ExplorerRepo{..} Explorer{..} = do
  minIndex <- getMinIndex
  outputs  <- getUspentOutputs minIndex (Limit $ toInteger $ Natural.naturalToInt limitOffset)
  _        <- putMinIndex $ Gix $ unGix minIndex + toInteger (length $ items outputs)
  pure $ items outputs
