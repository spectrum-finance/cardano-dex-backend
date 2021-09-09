module Tracker.Models.ExplorerModels

data ApiBlockchainInfo = ApiBlockchainInfo {
	bestHeight :: Height,
	lastBlockHash :: Hash
}

data ApiTxInfo = ApiTxInfo {
	id :: Id
}

data ApiFullTxOut = ApiFullTxOut

newtype Id = Id Int
	deriving (Eq)

newtype Height = Height Int
  deriving (Eq)

newtype Hash = Hash ByteString