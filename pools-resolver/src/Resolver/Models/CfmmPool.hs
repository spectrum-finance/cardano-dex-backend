module Resolver.Models.CfmmPool where

import RIO
newtype PoolId = PoolId { poolIdValue :: Int } deriving (Show)

newtype AssetAmount = AssetAmount { assetValue :: Int } deriving (Show)

newtype FeeAmount = FeeAmount { feeValue :: Int } deriving (Show)

newtype TxOutId = TxOutId { txOutId :: String } deriving (Show)

data TxOutRef = TxOutRef 
    { lastTxOutId :: TxOutId
    , lastConfirmedBoxGix :: Int
    } deriving (Show)

data CfmmPool = CfmmPool 
    { poolId :: PoolId
    , lp :: AssetAmount
    , x :: AssetAmount
    , y :: AssetAmount
    , fee :: FeeAmount
    , txOutRef :: TxOutRef
    } deriving (Show)

