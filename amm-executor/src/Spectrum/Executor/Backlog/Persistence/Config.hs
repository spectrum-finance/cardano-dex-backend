module Spectrum.Executor.Backlog.Persistence.Config
  ( BacklogStoreConfig(..)
  ) where

import GHC.Generics
  ( Generic )
import Dhall
  ( FromDhall )

data BacklogStoreConfig = BacklogStoreConfig
  { storePath       :: !FilePath
  , createIfMissing :: !Bool
  , persistent      :: !Bool
  }
  deriving (Generic, FromDhall)
