module Spectrum.Executor.EventSource.Persistence.LedgerHistory
  ( LedgerHistory(..)
  , mkRuntimeLedgerHistory
  ) where

import RIO
  ( ByteString, MonadThrow (throwM), ($>), newIORef, readIORef, writeIORef )

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Aeson
  ( ToJSON, encode, FromJSON, decode )

import Control.Monad.IO.Class
  ( MonadIO (liftIO) )

import Spectrum.Executor.Types
  ( ConcretePoint )
import Spectrum.Executor.EventSource.Persistence.Data.BlockLinks
  ( BlockLinks )
import Spectrum.Executor.EventSource.Persistence.Exception
  ( StorageDeserializationFailed(StorageDeserializationFailed) )
import System.Logging.Hlog (Logging (..), MakeLogging (..))

data LedgerHistory m = LedgerHistory
  { setTip      :: ConcretePoint -> m ()
  , getTip      :: m (Maybe ConcretePoint)
  , putBlock    :: ConcretePoint -> BlockLinks -> m ()
  , getBlock    :: ConcretePoint -> m (Maybe BlockLinks)
  , pointExists :: ConcretePoint -> m Bool
  , dropBlock   :: ConcretePoint -> m Bool
  }

mkRuntimeLedgerHistory :: MonadIO m => MakeLogging m m -> m (LedgerHistory m)
mkRuntimeLedgerHistory MakeLogging{..} = do
  store   <- liftIO $ newIORef mempty
  logging <- forComponent "LedgerHistory"
  pure $ attachLogging logging LedgerHistory
    { setTip = \p -> liftIO $ do
        s <- readIORef store
        writeIORef store $ Map.insert lastPointKey (encodeStrict p) s
    , getTip = liftIO $ do
        s <- readIORef store
        mapM decode' $ Map.lookup lastPointKey s
    , putBlock = \point blk -> liftIO $ do
        s <- readIORef store
        writeIORef store $ Map.insert (encodeStrict point) (encodeStrict blk) s
    , getBlock = \point -> liftIO $ do
        s <- readIORef store
        mapM decode' $ Map.lookup (encodeStrict point) s
    , pointExists = \point -> liftIO $ do
        s <- readIORef store
        pure $ Map.member (encodeStrict point) s
    , dropBlock = \point -> liftIO $ do
        s <- readIORef store
        let
          pkey   = encodeStrict point
          exists = Map.member pkey s
        if exists
          then writeIORef store (Map.delete pkey s) $> True
          else pure False
    }

attachLogging :: Monad m => Logging m -> LedgerHistory m -> LedgerHistory m
attachLogging Logging{..} LedgerHistory{..} =
  LedgerHistory
    { setTip = \point -> do
        infoM $ "setTip " <> show point
        r <- setTip point
        infoM $ "setTip " <> show point <> " -> " <> show r
        pure r
    , getTip = do
        infoM @String "getTip"
        r <- getTip
        infoM $ "getTip -> " <> show r
        pure r
    , putBlock = \point blk -> do
        infoM $ "putBlock " <> show point
        r <- putBlock point blk
        infoM $ "putBlock " <> show point <> " -> " <> show r
        pure r
    , getBlock = \point -> do
        infoM $ "getBlock " <> show point
        r <- getBlock point
        infoM $ "getBlock " <> show point <> " -> " <> show r
        pure r
    , pointExists = \point -> do
        infoM $ "pointExists " <> show point
        r <- pointExists point
        infoM $ "pointExists " <> show point <> " -> " <> show r
        pure r
    , dropBlock = \point -> do
        infoM $ "dropBlock " <> show point
        r <- dropBlock point
        infoM $ "dropBlock " <> show point <> " -> " <> show r
        pure r
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