module Tracker.Services.ExplorerService 
  ( ExplorerService(..)
  , mkExplorerService
  ) where 

import Prelude
import Tracker.Models.AppSettings
import GHC.Natural as Natural
import Explorer.Models
import Explorer.Service
import Tracker.Repository.ExplorerRepo
import Explorer.Types

data ExplorerService f = ExplorerService
 { getOutputs :: f [FullTxOut] 
 }

mkExplorerService 
  :: Monad f 
  => ExplorerSettings 
  -> ExplorerRepo f 
  -> Explorer f 
  -> ExplorerService f
mkExplorerService settings repo client = ExplorerService $ getOutputs' settings repo client

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
