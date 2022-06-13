module Spectrum.Executor.Data.PoolState
  ( PoolState(..)
  , Predicted(..)
  , Confirmed(..)
  , Unconfirmed(..)
  , type Pool
  ) where

import Data.Aeson
  ( ToJSON, FromJSON )
import GHC.Generics
  ( Generic )

import ErgoDex.State
  ( OnChain )
import qualified ErgoDex.Amm.Pool as Core

type Pool = OnChain Core.Pool

data PoolState
  = New
  | Consumed
  | Discarded
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Predicted a = Predicted
  { unPredicated :: a
  }
  deriving newtype (Eq, Generic, ToJSON, FromJSON)
  deriving Show

newtype Confirmed a = Confirmed
  { unConfirmed :: a
  }
  deriving newtype (Eq, Generic, ToJSON, FromJSON)
  deriving Show

newtype Unconfirmed a = Unconfirmed
  { unUnconfirmed :: a
  }
  deriving newtype (Eq, Generic, ToJSON, FromJSON)
  deriving Show
