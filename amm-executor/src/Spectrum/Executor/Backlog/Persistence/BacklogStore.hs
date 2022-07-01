module Spectrum.Executor.Backlog.Persistence.BacklogStore
  ( BacklogStore(..)
  , mkBacklogStore
  ) where

import qualified Database.RocksDB as Rocks

import Spectrum.Executor.Types (OrderId, orderId)
import Spectrum.Common.Persistence.Serialization
  ( serialize, deserializeM )
import System.Logging.Hlog (MakeLogging(MakeLogging, forComponent), Logging (Logging, infoM))
import Spectrum.Executor.Backlog.Persistence.Config (BacklogConfig(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Aeson (FromJSON)
import RIO hiding (drop)
import Spectrum.Executor.Backlog.Data.BacklogOrder (BacklogOrder (BacklogOrder, backlogOrder))
import Prelude hiding (drop)

data BacklogStore m = BacklogStore
  { put        :: BacklogOrder -> m ()
  , exists     :: BacklogOrder -> m Bool
  , dropOrder  :: OrderId -> m ()
  , get        :: OrderId -> m (Maybe BacklogOrder)
  , getAll     :: m [BacklogOrder]
  }

mkBacklogStore
  :: forall f m. (MonadIO f, MonadResource f, MonadIO m, MonadThrow m, MonadUnliftIO m)
  => MakeLogging f m
  -> BacklogConfig
  -> f (BacklogStore m)
mkBacklogStore MakeLogging{..} BacklogConfig{..} = do
  logging     <- forComponent "BacklogStore"
  (_, db)     <- Rocks.openBracket storePath
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
    { put = \BacklogOrder{..} ->
        put (serialize . orderId $ backlogOrder) (serialize backlogOrder)
    , exists = \BacklogOrder{..} -> exists . serialize . orderId $ backlogOrder
    , dropOrder = delete . serialize
    , get = get . serialize
    , getAll = bracket (Rocks.createIter db Rocks.defaultReadOptions) Rocks.releaseIter ((=<<) (mapM deserializeM) . Rocks.iterValues)
    }

attachLogging :: Monad m => Logging m -> BacklogStore m -> BacklogStore m
attachLogging Logging{..} BacklogStore{..}=
  BacklogStore
    { put = \order -> do
        infoM $ "put " <> show order
        r <- put order
        infoM $ "put " <> show order <> " -> " <> show r
        pure r
    , exists = \order -> do
        infoM $ "isExist " <> show order
        r <- exists order
        infoM $ "isExist " <> show order <> " -> " <> show r
        pure r
    , dropOrder = \oId -> do
        infoM $ "dropOrder " <> show oId
        r <- dropOrder oId
        infoM $ "dropOrder " <> show oId <> " -> " <> show r
        pure r
    , get = \oId -> do
        infoM $ "get " <> show oId
        r <- get oId
        infoM $ "get " <> show oId <> " -> " <> show r
        pure r
    , getAll = do
        infoM @String "getAll"
        r <- getAll
        infoM $ "getAll -> Size: " <> (show . length $ r)
        pure r
    }