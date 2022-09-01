module Spectrum.LedgerSync.Exception
  ( ChainSyncInitFailed(..)
  ) where

import Control.Exception (Exception)

newtype ChainSyncInitFailed = ChainSyncInitFailed String
  deriving stock (Show, Eq)
  deriving anyclass Exception
