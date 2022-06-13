module Spectrum.Common.Persistence.Exception
  ( StorageDeserializationFailed(..)
  ) where

import Control.Exception (Exception)

newtype StorageDeserializationFailed = StorageDeserializationFailed String
  deriving (Eq, Show)
  deriving anyclass Exception
