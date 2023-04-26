module Spectrum.Executor.Backlog.Persistence.BacklogStore
  ( BacklogStore(..)
  , mkBacklogStore
  ) where

import qualified Database.RocksDB as Rocks

import RIO     hiding (drop)
import Prelude hiding (drop)

import Data.Aeson       
  ( FromJSON )
import Control.Monad.Trans.Resource
  ( MonadResource )

import System.Logging.Hlog 
  ( MakeLogging(MakeLogging, forComponent), Logging (Logging, infoM, debugM) )

import Spectrum.Executor.Types 
  ( OrderId, orderId )
import Spectrum.Common.Persistence.Serialization
  ( serialize, deserializeM )
import Spectrum.Executor.Backlog.Persistence.Config 
  ( BacklogStoreConfig(..) )
import Spectrum.Executor.Backlog.Data.BacklogOrder 
  ( BacklogOrder (BacklogOrder, backlogOrder) )
import Spectrum.Prelude.Context
  ( HasType, askContext )

data BacklogStore m = BacklogStore
  { put       :: BacklogOrder -> m ()
  , exists    :: BacklogOrder -> m Bool
  , dropOrder :: OrderId -> m ()
  , get       :: OrderId -> m (Maybe BacklogOrder)
  , getAll    :: m [BacklogOrder]
  }

mkBacklogStore
  :: forall f m env. 
    ( MonadIO f
    , MonadResource f
    , MonadIO m
    , MonadThrow m
    , MonadUnliftIO m
    , MonadReader env f
    , HasType (MakeLogging f m) env
    , HasType BacklogStoreConfig env
    )
  => f (BacklogStore m)
mkBacklogStore = do
  MakeLogging{..}        <- askContext
  BacklogStoreConfig{..} <- askContext

  logging <- forComponent "BacklogStore"
  (_, db) <- Rocks.openBracket storePath
                Rocks.defaultOptions
                  { Rocks.createIfMissing = createIfMissing
                  }
  let
    get :: FromJSON a => ByteString -> m (Maybe a)
    get = (=<<) (mapM deserializeM) . Rocks.get db Rocks.defaultReadOptions
    exists :: ByteString -> m Bool
    exists k = Rocks.get db Rocks.defaultReadOptions k <&> isJust
    put = Rocks.put db Rocks.defaultWriteOptions
    delete = Rocks.delete @m db Rocks.defaultWriteOptions
  pure $ attachLogging logging BacklogStore
    { put       = \order@BacklogOrder{..} -> put (serialize . orderId $ backlogOrder) (serialize order)
    , exists    = \BacklogOrder{..} -> exists . serialize . orderId $ backlogOrder
    , dropOrder = delete . serialize
    , get       = get . serialize
    , getAll    = bracket (Rocks.createIter db Rocks.defaultReadOptions) Rocks.releaseIter ((=<<) (mapM deserializeM) . Rocks.iterValues)
    }

attachLogging :: Monad m => Logging m -> BacklogStore m -> BacklogStore m
attachLogging Logging{..} BacklogStore{..}=
  BacklogStore
    { put = \order -> do
        debugM $ "put " <> show order
        r <- put order
        debugM $ "put " <> show order <> " -> " <> show r
        pure r
    , exists = \order -> do
        debugM $ "isExist " <> show order
        r <- exists order
        debugM $ "isExist " <> show order <> " -> " <> show r
        pure r
    , dropOrder = \oId -> do
        debugM $ "dropOrder " <> show oId
        r <- dropOrder oId
        debugM $ "dropOrder " <> show oId <> " -> " <> show r
        pure r
    , get = \oId -> do
        debugM $ "get " <> show oId
        r <- get oId
        debugM $ "get " <> show oId <> " -> " <> show r
        pure r
    , getAll = do
        debugM @String "getAll"
        r <- getAll
        debugM $ "getAll -> Size: " <> (show . length $ r)
        pure r
    }