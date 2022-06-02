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
  ( BlockLinks (BlockLinks, blockPoint) )
import Spectrum.Executor.EventSource.Persistence.Exception
  ( StorageDeserializationFailed(StorageDeserializationFailed) )

data LedgerHistory m = LedgerHistory
  { putLastPoint :: ConcretePoint -> m ()
  , getLastPoint :: m (Maybe ConcretePoint)
  , putBlock     :: BlockLinks -> m ()
  , getBlock     :: ConcretePoint -> m (Maybe BlockLinks)
  , dropBlock    :: ConcretePoint -> m Bool
  }

mkRuntimeLedgerHistory :: MonadIO m => m (LedgerHistory m)
mkRuntimeLedgerHistory = liftIO $ do
  store <- newMVar mempty
  pure LedgerHistory
    { putLastPoint = \p -> liftIO $ do
        s <- readMVar store
        putMVar store $ Map.insert "k" (encodeStrict p) s
    , getLastPoint = liftIO $ do
        s <- readMVar store
        mapM decode' $ Map.lookup "k" s
    , putBlock = \blk@BlockLinks{..} -> liftIO $ do
        s <- readMVar store
        putMVar store $ Map.insert (encodeStrict blockPoint) (encodeStrict blk) s
    , getBlock = \point -> liftIO $ do
        s <- readMVar store
        mapM decode' $ Map.lookup (encodeStrict point) s
    , dropBlock = \point -> liftIO $ do
        s <- readMVar store
        let pkey = encodeStrict point
        blk <- mapM decode' $ Map.lookup pkey s
        case blk of
          Just BlockLinks{..} | blockPoint == point -> putMVar store (Map.delete pkey s) $> True
          _ -> pure False
    }

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = LBS.toStrict . encode

decode' :: (MonadThrow m, FromJSON a) => ByteString -> m a
decode' =
  maybe
    (throwM $ StorageDeserializationFailed "Cannot parse data from ledger storage")
    pure . decode . LBS.fromStrict
