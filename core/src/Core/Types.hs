module Core.Types
  ( OnChainIndexedEntity(..)
  , PredictedPool(..)
  , ConfirmedPool(..)
  , getPool
  , getPoolId
  ) where

import ErgoDex.Amm.Pool
import Cardano.Models
import Explorer.Types
import Data.Aeson        (FromJSON(..), ToJSON(..))
import RIO

data OnChainIndexedEntity a = OnChainIndexedEntity
  { entity              :: a
  , txOut               :: FullTxOut
  , lastConfirmedOutGix :: Gix
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype PredictedPool = PredictedPool (OnChainIndexedEntity Pool)
  deriving (Show, Generic, FromJSON, ToJSON)

newtype ConfirmedPool = ConfirmedPool (OnChainIndexedEntity Pool)
  deriving (Show, Generic, FromJSON, ToJSON)

getPool :: OnChainIndexedEntity Pool -> Pool
getPool OnChainIndexedEntity{entity=pool, ..} = pool

getPoolId :: OnChainIndexedEntity Pool -> PoolId
getPoolId OnChainIndexedEntity{entity=Pool{..}, ..} = poolId