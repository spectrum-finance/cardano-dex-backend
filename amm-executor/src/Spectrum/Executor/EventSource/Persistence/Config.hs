module Spectrum.Executor.EventSource.Persistence.Config
  ( LedgerStoreConfig(..)
  ) where

import GHC.Generics
  ( Generic )
import Dhall
  ( FromDhall )

data LedgerStoreConfig = LedgerStoreConfig
  { storePath       :: !FilePath
  , createIfMissing :: !Bool
  }
  deriving (Generic, FromDhall)
