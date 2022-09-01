module Spectrum.Executor.EventSource.Persistence.LedgerHistory
  ( LedgerHistory(..)
  , mkLedgerHistory
  , mkRuntimeLedgerHistory
  ) where

import RIO
  ( ByteString
  , ($>)
  , newIORef
  , readIORef
  , writeIORef
  , (<&>)
  , isJust, MonadTrans (lift)
  )

import qualified Data.Map as Map

import Control.Monad.IO.Class
  ( MonadIO )
import Control.Monad.Trans.Resource
  ( MonadResource, ResourceT )

import System.Logging.Hlog
  ( Logging (..), MakeLogging (..) )

import qualified Database.RocksDB as Rocks

import Spectrum.Executor.EventSource.Types
  ( ConcretePoint )
import Spectrum.Executor.EventSource.Persistence.Data.BlockLinks
  ( BlockLinks )
import Spectrum.Executor.EventSource.Persistence.Config
  ( LedgerStoreConfig (..) )
import Spectrum.Common.Persistence.Serialization (serialize, deserializeM)
import Control.Monad.Catch (MonadThrow)

data LedgerHistory m = LedgerHistory
  { setTip      :: ConcretePoint -> m ()
  , getTip      :: m (Maybe ConcretePoint)
  , putBlock    :: ConcretePoint -> BlockLinks -> m ()
  , getBlock    :: ConcretePoint -> m (Maybe BlockLinks)
  , pointExists :: ConcretePoint -> m Bool
  , dropBlock   :: ConcretePoint -> m Bool
  }

mkLedgerHistory
  :: (MonadIO f, MonadResource f, MonadIO m, MonadThrow m)
  => MakeLogging f m
  -> LedgerStoreConfig
  -> f (LedgerHistory m)
mkLedgerHistory MakeLogging{..} LedgerStoreConfig{..} = do
  logging <- forComponent "LedgerHistory"
  (_, db) <- Rocks.openBracket storePath
              Rocks.defaultOptions
                { Rocks.createIfMissing = createIfMissing
                }
  let
    readopts  = Rocks.defaultReadOptions
    writeopts = Rocks.defaultWriteOptions
  pure $ attachLogging logging LedgerHistory
    { setTip = Rocks.put db writeopts lastPointKey . serialize
    , getTip = Rocks.get db readopts lastPointKey >>= mapM deserializeM
    , putBlock = \point blk -> Rocks.put db writeopts (serialize point) (serialize blk)
    , getBlock = \point -> Rocks.get db readopts (serialize point) >>= mapM deserializeM
    , pointExists = \point -> Rocks.get db readopts (serialize point) <&> isJust
    , dropBlock = \point -> do
        let pkey = serialize point
        exists <- Rocks.get db readopts pkey <&> isJust
        if exists
          then Rocks.delete db writeopts pkey $> True
          else pure False
    }

-- | Runtime-only storage primarily for tests.
mkRuntimeLedgerHistory :: (MonadIO m, MonadThrow m) => MakeLogging m m -> m (LedgerHistory m)
mkRuntimeLedgerHistory MakeLogging{..} = do
  store   <- newIORef mempty
  logging <- forComponent "LedgerHistory"
  pure $ attachLogging logging LedgerHistory
    { setTip = \p -> do
        s <- readIORef store
        writeIORef store $ Map.insert lastPointKey (serialize p) s
    , getTip = do
        s <- readIORef store
        mapM deserializeM $ Map.lookup lastPointKey s
    , putBlock = \point blk -> do
        s <- readIORef store
        writeIORef store $ Map.insert (serialize point) (serialize blk) s
    , getBlock = \point -> do
        s <- readIORef store
        mapM deserializeM $ Map.lookup (serialize point) s
    , pointExists = \point -> do
        s <- readIORef store
        pure $ Map.member (serialize point) s
    , dropBlock = \point -> do
        s <- readIORef store
        let
          pkey   = serialize point
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
