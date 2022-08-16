module Spectrum.Common.Persistence.Serialization
  ( serialize
  , deserializeM
  ) where

import RIO
  ( ByteString, MonadThrow(..) )

import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
  ( ToJSON, encode, FromJSON, decode )
import Spectrum.Common.Persistence.Exception
    ( StorageDeserializationFailed(StorageDeserializationFailed) )

serialize :: ToJSON a => a -> ByteString
serialize = LBS.toStrict . encode

deserializeM :: (MonadThrow m, FromJSON a) => ByteString -> m a
deserializeM =
  maybe
    (throwM $ StorageDeserializationFailed "Cannot parse data from ledger storage")
    pure . decode . LBS.fromStrict
