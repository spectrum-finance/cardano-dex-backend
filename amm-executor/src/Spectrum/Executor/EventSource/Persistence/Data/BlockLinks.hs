module Spectrum.Executor.EventSource.Persistence.Data.BlockLinks where

import GHC.Generics
  ( Generic )

import qualified Data.Set as Set
import Data.Aeson
  ( ToJSON, FromJSON )

import Ledger
  ( TxId )
import Spectrum.Executor.Types
  ( ConcretePoint )

data BlockLinks = BlockLinks
  { blockPoint :: ConcretePoint
  , txIds      :: Set.Set TxId
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)
