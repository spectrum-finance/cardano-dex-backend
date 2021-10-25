module Resolver.Models.Types where

import ErgoDex.Amm.Pool
import Cardano.Models
import Explorer.Types
import Data.Aeson        (FromJSON(..), ToJSON(..))
import GHC.Generics      (Generic)

data OnChainIndexedEntity a = OnChainIndexedEntity
  { entity :: a
  , txOut :: FullTxOut
  , lastConfirmedOutGix :: Gix
  } deriving (Show, Generic, FromJSON, ToJSON)

type PredictedPool = OnChainIndexedEntity Pool

type ConfirmedPool = OnChainIndexedEntity Pool

getPool :: OnChainIndexedEntity Pool -> Pool
getPool OnChainIndexedEntity{entity=pool, ..} = pool

getPoolId :: OnChainIndexedEntity Pool -> PoolId
getPoolId OnChainIndexedEntity{entity=Pool{..}, ..} = poolId