{-# LANGUAGE PatternSynonyms #-}

module Spectrum.Executor.Data.State
  ( Predicted(..)
  , Confirmed(..)
  , Unconfirmed(..)
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
import CardanoTx.Models
  ( FullTxOut )

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

