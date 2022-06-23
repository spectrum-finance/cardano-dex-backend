module Spectrum.Executor.Backlog.Persistence.Config
  ( BacklogConfig(..)
  ) where

import GHC.Generics
  ( Generic )
import Dhall
  ( FromDhall )

data BacklogConfig = BacklogConfig
  { storePath       :: !FilePath
  , createIfMissing :: !Bool
  }
  deriving (Generic, FromDhall)
