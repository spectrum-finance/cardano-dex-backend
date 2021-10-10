module Tracker.Services.ExplorerService 
        ( ExplorerService(..)
        , mkExplorerService
        ) where

import qualified RIO
import Tracker.Models.ExplorerModels
import Prelude
import Tracker.Models.AppSettings
import Tracker.Clients.ExplorerClient
import Control.Concurrent.STM.TVar
import GHC.Natural as Natural

data ExplorerService = ExplorerService
 { getOutputs :: IO [ApiFullTxOut] 
 }

mkExplorerService :: ExplorerSettings -> ExplorerClient -> IO ExplorerService
mkExplorerService settings client = do
        offsetsT <- newTVarIO 0 --todo persist
        pure $ ExplorerService $ getOutputs' settings client offsetsT

-- todo: make newtypes for Int as minIndex
getOutputs' :: ExplorerSettings -> ExplorerClient -> TVar Int -> IO [ApiFullTxOut]
getOutputs' ExplorerSettings{..} ExplorerClient{..} offsetsT = do
        minIndex <- readTVarIO offsetsT
        outputs  <- getUspentOutputs minIndex (Natural.naturalToInt limitOffset)
        let nextMinIndex = minIndex + length outputs
        print $ "ExplorerService::nextMinIndex=" ++ show nextMinIndex
        RIO.atomically (modifyTVar offsetsT (\prevValue -> prevValue + length outputs))
        pure outputs
