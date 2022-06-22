module Spectrum.Executor.Types  
  ( PoolStateId(..)
  , OrderId(..)
  , poolId
  , poolStateId
  , orderId
  , type Pool
  , type Order
  , OrderWeight(..)
  , weightOrder
  -- re-exports
  , PoolId(..)
  ) where

import ErgoDex.State
  ( OnChain (OnChain) )
import qualified ErgoDex.Amm.Pool as Core
import qualified ErgoDex.Amm.Orders as Core

import Ledger
  ( TxOutRef )
import CardanoTx.Models (FullTxOut(fullTxOutRef, FullTxOut))
import ErgoDex.Amm.Pool (PoolId(..))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

newtype PoolStateId = PoolStateId
  { unPoolStateId :: TxOutRef
  }
  deriving newtype (Eq, Show)

newtype OrderId = OrderId TxOutRef
  deriving newtype (Eq, Show, FromJSON, ToJSON)
  deriving (Generic)

type Pool = OnChain Core.Pool

poolId :: Pool -> PoolId
poolId (OnChain _ Core.Pool{poolId}) = poolId

poolStateId :: Pool -> PoolStateId
poolStateId (OnChain FullTxOut{..} _) = PoolStateId fullTxOutRef

type Order = OnChain Core.AnyOrder

orderId :: Order -> OrderId
orderId (OnChain FullTxOut{..} _) = OrderId fullTxOutRef

newtype OrderWeight = OrderWeight Integer 
  deriving newtype (Eq, Ord, Show)

weightOrder :: Order -> OrderWeight
weightOrder ord = undefined
