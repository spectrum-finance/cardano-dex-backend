module Spectrum.Executor.Types
  ( PoolStateId(..)
  , OrderId(..)
  , poolStateId
  , orderId
  , type Pool
  , type Order
  , OrderWithCreationTime(..)
  , OrderWeight(..)
  , weightOrder
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

newtype PoolStateId = PoolStateId
  { unPoolStateId :: TxOutRef
  }
  deriving newtype (Eq, Show)

newtype OrderId = OrderId TxOutRef
  deriving newtype (Eq, Show, FromJSON, ToJSON, Ord)
  deriving (Generic)

type Pool = OnChain Core.Pool

poolStateId :: Pool -> PoolStateId
poolStateId (OnChain FullTxOut{..} _) = PoolStateId fullTxOutRef

type Order = OnChain Core.AnyOrder

orderId :: Order -> OrderId
orderId (OnChain FullTxOut{..} _) = OrderId fullTxOutRef

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
