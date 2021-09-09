module Tracker.Clients.ExplorerClient

import Tracker.Models.ExplorerModels
import RIO

data ExplorerClient env = ExplorerClient {
	getBlockchainInfo :: RIO env ApiBlockchainInfo
	getFullTxOutById :: Id -> RIO env (Maybe ApiFullTxOut)
	getTxsInBlock :: Id -> RIO env [ApiTxInfo]
}