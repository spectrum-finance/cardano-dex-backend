module Resolver.Models.CfmmPool where

import RIO
import Data.Aeson

newtype PoolId = PoolId { poolIdValue :: Int } deriving (Show, Generic, FromJSON)

newtype AssetAmount = AssetAmount { assetValue :: Int } deriving (Show, Generic, FromJSON)

newtype FeeAmount = FeeAmount { feeValue :: Int } deriving (Show, Generic, FromJSON)

newtype TxOutId = TxOutId { txOutId :: String } deriving (Show, Generic, FromJSON)

data TxOutRef = TxOutRef 
    { lastTxOutId :: TxOutId
    , lastConfirmedBoxGix :: Int
    } deriving (Show, Generic, FromJSON)

data CfmmPool = CfmmPool 
    { poolId :: PoolId
    , lp :: AssetAmount
    , x :: AssetAmount
    , y :: AssetAmount
    , fee :: FeeAmount
    , txOutRef :: TxOutRef
    } deriving (Show, Generic, FromJSON)

