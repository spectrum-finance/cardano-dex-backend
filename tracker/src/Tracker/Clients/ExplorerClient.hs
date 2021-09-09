module Tracker.Clients.ExplorerClient where

import Tracker.Models.ExplorerModels
import RIO
import Tracker.Models.AppSettings (HasHttpSettings(..))

data ExplorerClient env = ExplorerClient {
	getBlockchainInfo :: RIO env ApiBlockchainInfo,
	getFullTxOutById :: Id -> RIO env (Maybe ApiFullTxOut),
	getTxsInBlock :: Id -> RIO env [ApiTxInfo]
}

mkExplorerClient :: HasHttpSettings env => ExplorerClient env
mkExplorerClient = ExplorerClient getBlockchainInfo' getFullTxOutById' getTxsInBlock'

getBlockchainInfo' :: HasHttpSettings env => RIO env ApiBlockchainInfo
getBlockchainInfo' = undefined

getFullTxOutById' :: HasHttpSettings env => Id -> RIO env (Maybe ApiFullTxOut)
getFullTxOutById' id = undefined

getTxsInBlock' :: HasHttpSettings env => Id -> RIO env [ApiTxInfo]
getTxsInBlock' id = undefined