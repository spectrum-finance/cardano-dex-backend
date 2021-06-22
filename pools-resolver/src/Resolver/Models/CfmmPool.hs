module Resolver.Models.CfmmPool where

import RIO
newtype PoolId = PoolId { poolIdValue :: Int }

newtype AssetAmount = AssetAmount { assetValue :: Int }

newtype FeeAmount = FeeAmount { feeValue :: Int }

newtype TxOutId = TxOutId { txOutId :: String }

data TxOutRef = TxOutRef 
    { lastTxOutId :: TxOutId
    , lastConfirmedBoxGix :: Int
    }

data Pool = CfmmPool 
    { poolId :: PoolId
    , lp :: AssetAmount
    , x :: AssetAmount
    , y :: AssetAmount
    , fee :: FeeAmount
    , txOutRef :: TxOutRef
    }

