module Tracker.Models.ExplorerModels where

import Data.String
import Plutus.V1.Ledger.Scripts ( Datum )
import Data.Aeson (FromJSON)
import Prelude
import GHC.Generics

--todo add newtypes

data Items = Items
	{ items :: [ApiFullTxOut]
	, total :: Int 
	} deriving (Show, Generic, FromJSON)

-- explorer should return ref as TxId plus Index

data ApiFullTxOut = ApiFullTxOut 
	{ ref :: String
	, index :: Integer
	, addr :: String
	, jsValue :: String
	, data' :: Maybe Datum
	, assets :: [OutputAsset]
	} deriving (Show, Generic, FromJSON)

data OutputAsset = OutputAsset
	{ name :: String
	, quantity :: Integer
	, policy :: String
	} deriving (Show, Generic, FromJSON)