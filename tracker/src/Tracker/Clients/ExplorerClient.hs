module Tracker.Clients.ExplorerClient where

import Tracker.Models.ExplorerModels
import RIO
import Tracker.Models.AppSettings (HasExplorerSettings(..))

data ExplorerClient env = ExplorerClient {
	getBlockchainInfo :: RIO env ApiBlockchainInfo,
	getFullTxOutById :: Id -> RIO env (Maybe ApiFullTxOut),
	getTxsInBlock :: Id -> RIO env [ApiTxInfo]
}

mkExplorerClient :: HasExplorerSettings env => ExplorerClient env
mkExplorerClient = ExplorerClient getBlockchainInfo' getFullTxOutById' getTxsInBlock'

getBlockchainInfo' :: HasExplorerSettings env => RIO env ApiBlockchainInfo
getBlockchainInfo' = undefined

getFullTxOutById' :: HasExplorerSettings env => Id -> RIO env (Maybe ApiFullTxOut)
getFullTxOutById' id = undefined

getTxsInBlock' :: HasExplorerSettings env => Id -> RIO env [ApiTxInfo]
getTxsInBlock' id = undefined