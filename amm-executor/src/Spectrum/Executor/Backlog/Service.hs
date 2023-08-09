module Spectrum.Executor.Backlog.Service
  ( BacklogService(..)
  , mkBacklogService
  , mkBacklogService'
  ) where

import           Prelude         hiding (drop)
import qualified Data.PQueue.Max as PQ
import qualified Data.List       as List
import qualified Data.Sequence   as Seq

import Control.Monad.IO.Class
  ( MonadIO )
import System.Random
  ( randomRIO )
import RIO
  ( (<&>), modifyIORef', newIORef, IORef, MonadReader, atomicModifyIORef', readIORef )
import RIO.Time
  ( getCurrentTime, diffUTCTime )

import System.Logging.Hlog
  ( MakeLogging(MakeLogging, forComponent), Logging (Logging, infoM, debugM) )

import Spectrum.Prelude.HigherKind
  ( LiftK(liftK) )
import Spectrum.Executor.Backlog.Data.BacklogOrder
  ( mkWeightedOrder, WeightedOrder (WeightedOrder), BacklogOrder (BacklogOrder, backlogOrder, orderTimestamp) )
import Spectrum.Executor.Backlog.Persistence.BacklogStore
  ( BacklogStore(BacklogStore, exists, get, dropOrder, put, getAll) )
import Spectrum.Executor.Backlog.Config
  ( BacklogServiceConfig (BacklogServiceConfig, orderLifetime, orderExecTime, suspendedPropability) )
import Spectrum.Executor.Data.OrderState
  ( OrderState (..), OrderInState (PendingOrder, SuspendedOrder, InProgressOrder) )
import Spectrum.Executor.Types
  ( OrderId, OrderWithCreationTime (OrderWithCreationTime), orderId )
import Spectrum.Prelude.Context
  ( HasType, askContext )
import Control.Monad.Trans.Resource
  ( MonadResource )
import Control.Monad.IO.Unlift
  ( MonadUnliftIO )
import GHC.Natural (naturalToInt)

data BacklogService m = BacklogService
  { put        :: OrderInState 'Pending -> m ()
  , suspend    :: OrderInState 'Suspended -> m Bool
  , checkLater :: OrderInState 'InProgress -> m Bool
  , tryAcquire :: m (Maybe OrderWithCreationTime)
  , drop       :: OrderId -> m ()
  }

mkBacklogService
  :: forall f m env.
    ( MonadIO f
    , MonadUnliftIO m
    , LiftK m f
    , MonadReader env f
    , HasType (MakeLogging f m) env
    , HasType BacklogServiceConfig env
    )
  => BacklogStore m
  -> f (BacklogService m)
mkBacklogService store = do
  mklog  <- askContext
  config <- askContext
  mkBacklogService' mklog config store

mkBacklogService'
  :: forall f m. (MonadIO f, MonadIO m, LiftK m f)
  => MakeLogging f m
  -> BacklogServiceConfig
  -> BacklogStore m
  -> f (BacklogService m)
mkBacklogService' MakeLogging{..} config store@BacklogStore{..} = do
  logging@Logging{..} <- forComponent "Bots.BacklogService"
  -- those queues should be shared with Backlog.Proceess (to make live updates). So maybe worth extracting them into separate module. e.g. BacklogStore
  pendingPQ   <- newIORef PQ.empty  -- ordered by weight; new orders
  suspendedPQ <- newIORef PQ.empty  -- ordered by weight; failed orders, waiting for retry (retries are performed with some constant probability, e.g. 5%) 
  toRevisitQ  <- newIORef Seq.empty -- regular queue; successully submitted orders. Left orders should be re-executed in X minutes. Normally successfully confirmed orders are eliminated from this queue.
  _           <- liftK $ recover store config pendingPQ
  pure $ attachLogging logging BacklogService
    { put = \(PendingOrder order timestamp) -> do       
        existsInStore <- exists $ BacklogOrder timestamp order
        if existsInStore
          then infoM ("Order " ++ show (orderId order) ++ " already exists in storage. Ignore it") >> pure ()
          else infoM ("Order " ++ show (orderId order) ++ " doesn't exists in storage. Put it") >> put (BacklogOrder timestamp order) >> modifyIORef' pendingPQ (PQ.insert $ mkWeightedOrder order timestamp)
    , suspend = \(SuspendedOrder order timestamp) -> do
        existsInStore <- exists $ BacklogOrder timestamp order
        if existsInStore
          then modifyIORef' suspendedPQ (PQ.insert $ mkWeightedOrder order timestamp) >> pure True
          else pure False
    , checkLater = \(InProgressOrder order timestamp) -> do
        existsInStore <- exists $ BacklogOrder timestamp order
        if existsInStore
          then modifyIORef' toRevisitQ (mkWeightedOrder order timestamp Seq.<|) >> pure True
          else pure False
    , tryAcquire = do
        revisitOrders config store pendingPQ toRevisitQ
        getMaxWeightedOrder' config store logging pendingPQ suspendedPQ
    , drop = dropOrder
    }

revisitOrders
  :: (MonadIO m)
  => BacklogServiceConfig
  -> BacklogStore m
  -> IORef (PQ.MaxQueue WeightedOrder)
  -> IORef (Seq.Seq WeightedOrder)
  -> m ()
revisitOrders BacklogServiceConfig{..} BacklogStore{..} pendingQueueRef toRevisitSeqRef = do
  currentTime       <- getCurrentTime
  revisited2process <-
    atomicModifyIORef'
      toRevisitSeqRef
      (Seq.spanl (\(WeightedOrder _ _ oTime) -> diffUTCTime currentTime oTime < orderExecTime))
  mapM_ (\order@(WeightedOrder oId _ oTime) ->
          if diffUTCTime currentTime oTime < orderLifetime
            then modifyIORef' pendingQueueRef (PQ.insert order)
            else dropOrder oId
       ) revisited2process

getMaxWeightedOrder'
  :: forall m. (MonadIO m)
  => BacklogServiceConfig
  -> BacklogStore m
  -> Logging m
  -> IORef (PQ.MaxQueue WeightedOrder)
  -> IORef (PQ.MaxQueue WeightedOrder)
  -> m (Maybe OrderWithCreationTime)
getMaxWeightedOrder' cfg@BacklogServiceConfig{..} store l@Logging{..} pendingQueueRef suspendedQueueRef = do
  randomInt <- randomRIO (1, 100) :: m Int
  if randomInt > naturalToInt suspendedPropability
    then (infoM @String "Going to get from pending") >> getMaxOrderFromQueue cfg store l pendingQueueRef
    else (infoM @String "Going to get from suspended") >> getMaxOrderFromQueue cfg store l suspendedQueueRef

getMaxOrderFromQueue
  :: MonadIO m
  => BacklogServiceConfig
  -> BacklogStore m
  -> Logging m
  -> IORef (PQ.MaxQueue WeightedOrder)
  -> m (Maybe OrderWithCreationTime)
getMaxOrderFromQueue cfg@BacklogServiceConfig{..} store@BacklogStore{..} l@Logging{..} queueRef = do
  currentTime <- getCurrentTime
  infoM $ "Current time: " ++ show currentTime
  queue <- readIORef queueRef
  infoM $ "Current queue: " ++ show queue
  wOrderM     <- atomicModifyIORef' queueRef (\queue -> case PQ.maxView queue of
      Just (order, newQueue) -> (newQueue, Just order)
      Nothing -> (queue, Nothing)
    )
  infoM $ "wOrderM: " ++ show wOrderM
  case wOrderM of
    Just (WeightedOrder oId _ oTime) -> do
      if diffUTCTime currentTime oTime > orderLifetime
      then infoM ("Order is outdated. OrderLifeTime: " ++ show orderLifetime ++ ". Diff between current:" ++ show (diffUTCTime currentTime oTime) ) >> dropOrder oId >> getMaxOrderFromQueue cfg store l queueRef
      else get oId <&> fmap (\order -> OrderWithCreationTime (backlogOrder order) oTime)
    Nothing -> pure Nothing

recover
  :: (MonadIO m)
  => BacklogStore m
  -> BacklogServiceConfig
  -> IORef (PQ.MaxQueue WeightedOrder)
  -> m ()
recover BacklogStore{..} BacklogServiceConfig{..} pendingQueueRef = do
  ordersInDb  <- getAll
  currentTime <- getCurrentTime
  let
    filteredOrders = List.filter (\BacklogOrder{..} -> diffUTCTime currentTime orderTimestamp < orderExecTime) ordersInDb
  modifyIORef'
    pendingQueueRef
    (\queue -> foldr (\BacklogOrder{..} -> PQ.insert (mkWeightedOrder backlogOrder orderTimestamp)) queue filteredOrders)

attachLogging :: Monad m => Logging m -> BacklogService m -> BacklogService m
attachLogging Logging{..} BacklogService{..} =
  BacklogService
    { put = \order -> do
        infoM $ "put " <> show order
        r <- put order
        infoM $ "put " <> show order <> " -> " <> show r
        pure r
    , suspend = \order -> do
        infoM $ "suspend " <> show order
        r <- suspend order
        infoM $ "suspend " <> show order <> " -> " <> show r
        pure r
    , checkLater = \order -> do
        infoM $ "checkLater " <> show order
        r <- checkLater order
        infoM $ "checkLater " <> show order <> " -> " <> show r
        pure r
    , tryAcquire = do
        debugM @String "tryAcquire"
        r <- tryAcquire
        debugM $ "tryAcquire -> " <> show r
        pure r
    , drop = \orderId -> do
        infoM $ "drop " <> show orderId
        r <- drop orderId
        infoM $ "drop " <> show orderId <> " -> " <> show r
        pure r
    }
