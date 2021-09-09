module Tracker.Clients.ExplorerClient

import Tracker.Models.ExplorerModels
import RIO

data ExplorerClient env = ExplorerClient {
	getBlockchainInfo :: RIO env ApiBlockchainInfo
	getFullTxOutById :: Id -> RIO env (Maybe ApiFullTxOut)
	getTxsInBlock :: Id -> RIO env [ApiTxInfo]
}

mkExplorerClient :: HasHttpSettings env => ExplorerClient
mkExplorerClient = ExplorerClient getBlockchainInfo' getFullTxOutById' getTxsInBlock'

getBlockchainInfo' :: HasHttpSettings env => RIO env ApiBlockchainInfo
getBlockchainInfo' = undefined

getFullTxOutById' :: HasHttpSettings env => Id -> RIO env (Maybe ApiFullTxOut)
getFullTxOutById' id = undefined

getTxsInBlock' :: HasHttpSettings env => Id -> RIO env [ApiTxInfo]
getTxsInBlock' id = undefined