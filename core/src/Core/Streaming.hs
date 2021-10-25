module Core.Streaming
  ( ConfirmedOrderEvent(..)
  , ConfirmedPoolEvent(..)
  ) where

import Data.Aeson
import GHC.Generics

import ErgoDex.Amm.Orders
import ErgoDex.State
import ErgoDex.Amm.Pool

import Prelude 

data ConfirmedOrderEvent = ConfirmedOrderEvent
  { order :: Confirmed AnyOrder
  , gix   :: Integer --todo Gix
  } deriving (Generic, FromJSON, ToJSON)

data ConfirmedPoolEvent = ConfirmedPoolEvent
  { pool :: Confirmed Pool
  , gix  :: Integer --todo Gix
  } deriving (Generic, FromJSON, ToJSON)