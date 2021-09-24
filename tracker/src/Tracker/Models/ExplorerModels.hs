{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DuplicateRecordFields     #-}

module Tracker.Models.ExplorerModels where

import Data.ByteString
import Data.Int
import Data.Eq
import Data.String
import Data.Aeson (FromJSON)
import Prelude
import GHC.Generics

newtype Id = Id { value :: Integer }
	deriving (Show, Generic, FromJSON)

newtype Height = Height { value :: Integer }
  deriving (Eq, Show, Generic, FromJSON, Ord)

newtype Hash = Hash String
	deriving (Show, Generic, FromJSON)

data ApiBlockchainInfo = ApiBlockchainInfo {
	bestId :: Height,
	blockHash :: Hash,
	epochNo :: Integer,
	slotNo :: Integer,
	slotLeader :: Hash,
	txCount :: Integer,
	time :: String
} deriving (Show, Generic, FromJSON)

data Transaction = Transaction {
	blockHash :: Hash,
	txHash :: String,
	outputs :: [ApiFullTxOut]
}

data ApiTxInfo = ApiTxInfo {
	id :: Id
} deriving (Show, Generic, FromJSON)

data ApiFullTxOut = ApiFullTxOut {
  outGId           :: GId,
  refId            :: TxId,
  refIdx           :: Integer,
  txOutAddress     :: AddressPubKeyHash,
  jsValue          :: Value,
  'data'           :: Datum
} deriving (Show, Generic, FromJSON)