module Tracker.Services.ExplorerService 
        ( ExplorerService(..)
        , mkExplorerService
        ) where

import qualified RIO
-- import Tracker.Models.ExplorerModels
import Prelude
import Tracker.Models.AppSettings
import Control.Concurrent.STM.TVar
import GHC.Natural as Natural
import Explorer.Models
import Explorer.Service

data ExplorerService f = ExplorerService
 { getOutputs :: f [FullTxOut] 
 }

mkExplorerService :: ExplorerSettings -> Explorer f -> ExplorerService f
mkExplorerService settings client = undefined -- do
        -- offsetsT <- newTVarIO 0 --todo persist
        -- pure $ ExplorerService $ getOutputs' settings client offsetsT

-- todo: make newtypes for Int as minIndex
-- getOutputs' :: ExplorerSettings -> ExplorerClient -> TVar Int -> IO [FullTxOut]
-- getOutputs' ExplorerSettings{..} ExplorerClient{..} offsetsT = do
--         minIndex <- readTVarIO offsetsT
--         outputs  <- getUspentOutputs minIndex (Natural.naturalToInt limitOffset)
--         let nextMinIndex = minIndex + length outputs
--         print $ "ExplorerService::nextMinIndex=" ++ show nextMinIndex
--         RIO.atomically (modifyTVar offsetsT (\prevValue -> prevValue + length outputs))
--         pure outputs
