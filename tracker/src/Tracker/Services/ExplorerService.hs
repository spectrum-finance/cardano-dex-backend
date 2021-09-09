module Tracker.Services.ExplorerService where

import RIO

data ExplorerService env = ExplorerService {
	getBestHeight :: RIO env Int
	getNewOutputs :: RIO env [ApiFullTxOut]
}

mkExplorerService :: ExplorerService env
mkExplorerService = ExplorerService getBestHeight' getNewOutputs'

getBestHeight' :: RIO env Int
getBestHeight' = undefined

getNewOutputs' :: RIO env [ApiFullTxOut]
getNewOutputs' = undefined
