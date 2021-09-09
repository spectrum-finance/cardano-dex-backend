module Tracker.Models.ExplorerModels

data ApiBlockchainInfo = ApiBlockchainInfo {
	bestHeight :: Int,
	lastBlockHash :: ByteString
}

data ApiTxInfo = ApiTxInfo {
	id :: Id
}

data ApiFullTxOut = ApiFullTxOut

newtype Id = Id Int
	deriving (Eq)