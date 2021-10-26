module Tracker.Services.TrackerService 
  ( TrackerService(..)
  , mkTrackerService
  ) where 

import Prelude
import Tracker.Models.AppSettings
import GHC.Natural as Natural
import Explorer.Models
import Explorer.Service
import Tracker.Repository.ExplorerRepo
import Explorer.Types

data TrackerService f = TrackerService
 { getOutputs :: f [FullTxOut] 
 }

mkTrackerService 
  :: Monad f 
  => ExplorerSettings 
  -> ExplorerRepo f 
  -> Explorer f 
  -> TrackerService f
mkTrackerService settings repo client = TrackerService $ getOutputs' settings repo client

getOutputs'
  :: Monad f 
  => ExplorerSettings
  -> ExplorerRepo f 
  -> Explorer f 
  -> f [FullTxOut]
getOutputs' ExplorerSettings{..} ExplorerRepo{..} Explorer{..} = do
  minIndex <- getMinIndex
  outputs  <- getUspentOutputs minIndex (Limit $ toInteger $ Natural.naturalToInt limitOffset)
  _        <- putMinIndex $ Gix $ unGix minIndex + toInteger (length $ items outputs)
  pure $ items outputs
