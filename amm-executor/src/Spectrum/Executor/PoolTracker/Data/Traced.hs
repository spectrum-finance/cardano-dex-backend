module Spectrum.Executor.PoolTracker.Data.Traced
  ( Traced(..)
  ) where

import GHC.Generics
  ( Generic )
import Data.Aeson
  ( FromJSON, ToJSON )
import Ledger
  ( TxOutRef )

-- | State of an on-chain entity linked to a prev version of inself.
data Traced a = Traced
  { tracedState  :: a
  , prevTxOutRef :: TxOutRef
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
