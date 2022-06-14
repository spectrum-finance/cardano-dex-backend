{-# LANGUAGE PatternSynonyms #-}

module Spectrum.Executor.Data.PoolState
  ( PoolState(..)
  , Predicted(..)
  , Confirmed(..)
  , Unconfirmed(..)
  , type Pool
  , pattern PredictedState
  , pattern ConfirmedState
  , pattern UnconfirmedState
  ) where

import Data.Aeson
  ( ToJSON, FromJSON )
import GHC.Generics
  ( Generic )

import ErgoDex.State
  ( OnChain (OnChain) )
import qualified ErgoDex.Amm.Pool as Core
import CardanoTx.Models (FullTxOut)

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

pattern PredictedState :: FullTxOut -> a -> Predicted (OnChain a)
pattern PredictedState out a = Predicted (OnChain out a)

newtype Confirmed a = Confirmed
  { unConfirmed :: a
  }
  deriving newtype (Eq, Generic, ToJSON, FromJSON)
  deriving Show

pattern ConfirmedState :: FullTxOut -> a -> Confirmed (OnChain a)
pattern ConfirmedState out a = Confirmed (OnChain out a)

newtype Unconfirmed a = Unconfirmed
  { unUnconfirmed :: a
  }
  deriving newtype (Eq, Generic, ToJSON, FromJSON)
  deriving Show

pattern UnconfirmedState :: FullTxOut -> a -> Unconfirmed (OnChain a)
pattern UnconfirmedState out a = Unconfirmed (OnChain out a)
