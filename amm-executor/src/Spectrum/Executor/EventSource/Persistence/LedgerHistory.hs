module Spectrum.Executor.EventSource.Persistence.LedgerHistory
  ( LedgerHistory(..)
  , mkRuntimeLedgerHistory
  ) where

import RIO
  ( ByteString, MonadThrow (throwM), ($>) )

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Aeson
  ( ToJSON, encode, FromJSON, decode )

import Control.Concurrent
  ( newMVar, putMVar, readMVar )
import Control.Monad.IO.Class
  ( MonadIO (liftIO) )

import Spectrum.Executor.Types
  ( ConcretePoint )
import Spectrum.Executor.EventSource.Persistence.Data.BlockLinks
  ( BlockLinks )
import Spectrum.Executor.EventSource.Persistence.Exception
  ( StorageDeserializationFailed(StorageDeserializationFailed) )

data LedgerHistory m = LedgerHistory
  { setTip :: ConcretePoint -> m ()
  , getTip :: m (Maybe ConcretePoint)
  , putBlock     :: ConcretePoint -> BlockLinks -> m ()
  , getBlock     :: ConcretePoint -> m (Maybe BlockLinks)
  , pointExists  :: ConcretePoint -> m Bool
  , dropBlock    :: ConcretePoint -> m Bool
  }

mkRuntimeLedgerHistory :: MonadIO m => m (LedgerHistory m)
mkRuntimeLedgerHistory = liftIO $ do
  store <- newMVar mempty
  pure LedgerHistory
    { setTip = \p -> liftIO $ do
        s <- readMVar store
        putMVar store $ Map.insert lastPointKey (encodeStrict p) s
    , getTip = liftIO $ do
        s <- readMVar store
        mapM decode' $ Map.lookup lastPointKey s
    , putBlock = \point blk -> liftIO $ do
        s <- readMVar store
        putMVar store $ Map.insert (encodeStrict point) (encodeStrict blk) s
    , getBlock = \point -> liftIO $ do
        s <- readMVar store
        mapM decode' $ Map.lookup (encodeStrict point) s
    , pointExists = \point -> liftIO $ do
        s <- readMVar store
        pure $ Map.member (encodeStrict point) s
    , dropBlock = \point -> liftIO $ do
        s <- readMVar store
        let
          pkey   = encodeStrict point
          exists = Map.member pkey s
        if exists
          then putMVar store (Map.delete pkey s) $> True
          else pure False
    }

lastPointKey :: ByteString
lastPointKey = "lastPoint"

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = LBS.toStrict . encode

decode' :: (MonadThrow m, FromJSON a) => ByteString -> m a
decode' =
  maybe
    (throwM $ StorageDeserializationFailed "Cannot parse data from ledger storage")
    pure . decode . LBS.fromStrict
