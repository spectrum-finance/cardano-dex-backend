module Spectrum.Executor.Types
  ( PoolStateId(..)
  , OrderId(..)
  , poolStateId
  , orderId
  , orderRef
  , Pool(..)
  , type Order
  , OrderWithCreationTime(..)
  , PoolVersion(..)
  , OrderWeight(..)
  , weightOrder
  , extractPoolId
  -- re-exports
  , PoolId(..)
  ) where

import GHC.Generics 
  ( Generic )
import Data.Aeson 
  ( FromJSON, ToJSON )

import Ledger
  ( TxOutRef )

import ErgoDex.State
  ( OnChain (OnChain) )
import qualified ErgoDex.Amm.Pool as Core
import qualified ErgoDex.Amm.Orders as Core
import CardanoTx.Models 
  ( FullTxOut(fullTxOutRef, FullTxOut) )
import ErgoDex.Amm.Pool 
  ( PoolId(..) )
import ErgoDex.Amm.Orders 
  ( OrderAction(SwapAction, DepositAction, RedeemAction), Swap (Swap), Deposit (Deposit), Redeem (Redeem) )
import ErgoDex.Types 
  ( unExFee, exFeePerTokenDen, exFeePerTokenNum )
import ErgoDex.Contracts.Types 
  ( Amount(unAmount) )
import RIO.Time (UTCTime)
import ErgoDex.Validators (Version)

newtype PoolStateId = PoolStateId
  { unPoolStateId :: TxOutRef
  }
  deriving newtype (Eq, Show)

newtype OrderId = OrderId TxOutRef
  deriving newtype (Eq, Show, FromJSON, ToJSON, Ord)
  deriving (Generic)

data PoolVersion = PoolVersionV1 | PoolVersionV2
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

data Pool = Pool (OnChain Core.Pool) Version
  deriving (Generic, Eq, Show, FromJSON, ToJSON)

poolStateId :: Pool -> PoolStateId
poolStateId (Pool (OnChain FullTxOut{..} _) _) = PoolStateId fullTxOutRef

extractPoolId :: Pool -> PoolId
extractPoolId (Pool (OnChain _ Core.Pool{poolId}) _) = poolId

extractUnderlyingPool :: Pool -> Core.Pool
extractUnderlyingPool (Pool (OnChain _ origPool) _) = origPool

type Order = OnChain Core.AnyOrder

orderId :: Order -> OrderId
orderId (OnChain FullTxOut{..} _) = OrderId fullTxOutRef

orderRef :: OrderId -> TxOutRef
orderRef (OrderId ref) = ref

newtype OrderWeight = OrderWeight Integer
  deriving newtype (Eq, Ord, Show)

data OrderWithCreationTime = OrderWithCreationTime Order UTCTime
  deriving (Show)

weightOrder :: Order -> OrderWeight
weightOrder (OnChain _ Core.AnyOrder{..}) =
  case anyOrderAction of
    SwapAction Swap{..}       -> OrderWeight $ unAmount swapMinQuoteOut * exFeePerTokenNum swapExFee `div` exFeePerTokenDen  swapExFee
    DepositAction Deposit{..} -> OrderWeight . unAmount . unExFee $ depositExFee
    RedeemAction Redeem{..}   -> OrderWeight . unAmount . unExFee $ redeemExFee
