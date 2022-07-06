{-# LANGUAGE RecordWildCards #-}
module Tests.Backlog.PersistenceMock where

import Spectrum.Executor.Backlog.Persistence.BacklogStore
import Spectrum.Executor.Types (OrderId, orderId)
import Spectrum.Executor.Backlog.Data.BacklogOrder (BacklogOrder (BacklogOrder, backlogOrder))
import RIO
    ( MonadIO, (<&>), isJust, modifyIORef, newIORef, readIORef, IORef)
import qualified Data.Map as Map

mkMockStorage :: (MonadIO m, MonadIO i) => i (BacklogStore m)
mkMockStorage = do
  mapRef <- newIORef mempty
  pure $ BacklogStore
    { put        =  put' mapRef
    , exists     =  exists' mapRef
    , dropOrder  =  dropOrder' mapRef
    , get        =  get' mapRef
    , getAll     =  getAll' mapRef
    }

put' :: (MonadIO m) => IORef (Map.Map OrderId BacklogOrder) -> BacklogOrder -> m ()
put' mapRef order@BacklogOrder{..} = modifyIORef mapRef (Map.insert (orderId backlogOrder) order)

exists' :: (MonadIO m) => IORef (Map.Map OrderId BacklogOrder) -> BacklogOrder -> m Bool
exists' mapRef BacklogOrder{..} = readIORef mapRef <&> isJust . Map.lookup (orderId backlogOrder)

dropOrder' :: (MonadIO m) => IORef (Map.Map OrderId BacklogOrder) -> OrderId -> m ()
dropOrder' mapRef oId = modifyIORef mapRef (Map.delete oId)

get' :: (MonadIO m) => IORef (Map.Map OrderId BacklogOrder) -> OrderId -> m (Maybe BacklogOrder)
get' mapRef oId = readIORef mapRef <&> Map.lookup oId

getAll' :: (MonadIO m) => IORef (Map.Map OrderId BacklogOrder) -> m [BacklogOrder]
getAll' mapRef = readIORef mapRef <&> Map.elems