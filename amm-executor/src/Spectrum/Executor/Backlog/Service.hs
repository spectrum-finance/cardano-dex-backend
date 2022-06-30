module Spectrum.Executor.Backlog.Service
  ( BacklogService(..)
  , mkBacklogService
  ) where


import qualified Data.PQueue.Max as PQ
import qualified Data.List       as List
import qualified Data.Sequence   as Seq

import Spectrum.Executor.Data.OrderState (OrderState (..), OrderInState (PendingOrder, SuspendedOrder, InProgressOrder))
import Spectrum.Executor.Types (Order, OrderId)

import System.Logging.Hlog (MakeLogging(MakeLogging, forComponent), Logging (Logging, infoM))
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource ( MonadResource )
import RIO hiding (drop)
import Spectrum.Executor.Backlog.Data.BacklogOrder (mkWeightedOrderWithTimestamp, WeightedOrderWithTimestamp (WeightedOrderWithTimestamp), BacklogOrder (BacklogOrder, backlogOrder, orderTimestamp))
import Spectrum.Executor.Backlog.Persistence.BacklogStore (BacklogStore(BacklogStore, exists, get, drop, put, getAll))
import Prelude hiding (drop)
import Spectrum.Executor.Backlog.Config (BacklogServiceConfig (BacklogServiceConfig, orderLifetime, orderExecTime))
import RIO.Time (getCurrentTime, diffUTCTime)
import Spectrum.HigherKind (LiftK(liftK))

data BacklogService m = BacklogService
  { put        :: OrderInState 'Pending -> m ()
  , suspend    :: OrderInState 'Suspended -> m Bool
  , checkLater :: OrderInState 'InProgress -> m Bool
  , tryAcquire :: m (Maybe Order)
  , drop       :: OrderId -> m ()
  }

mkBacklogService
  :: forall f m. (MonadIO f, MonadResource f, MonadIO m, LiftK m f)
  => MakeLogging f m
  -> BacklogServiceConfig
  -> BacklogStore m
  -> f (BacklogService m)
mkBacklogService MakeLogging{..} config store@BacklogStore{..} = do
  logging     <- forComponent "BacklogService"
  -- those queues should be shared with Backlog.Proceess (to make live updates). So maybe worth extracting them into separate module. e.g. BacklogStore
  pendingPQ   <- newIORef PQ.empty  -- ordered by weight; new orders
  suspendedPQ <- newIORef PQ.empty  -- ordered by weight; failed orders, waiting for retry (retries are performed with some constant probability, e.g. 5%) 
  toRevisitQ  <- newIORef Seq.empty -- regular queue; successully submitted orders. Left orders should be re-executed in X minutes. Normally successfully confirmed orders are eliminated from this queue.
  _           <- liftK $ recover store config pendingPQ
  pure $ attachLogging logging BacklogService
    { put = \(PendingOrder order timestamp) -> do
        put $ BacklogOrder timestamp order
        modifyIORef pendingPQ (PQ.insert $ mkWeightedOrderWithTimestamp order timestamp)
    , suspend = \(SuspendedOrder order timestamp) -> do
        modifyIORef suspendedPQ (PQ.insert $ mkWeightedOrderWithTimestamp order timestamp)
        exists $ BacklogOrder timestamp order
    , checkLater = \(InProgressOrder order timestamp) -> do
        modifyIORef toRevisitQ (mkWeightedOrderWithTimestamp order timestamp Seq.<|)
        exists $ BacklogOrder timestamp order
    , tryAcquire = do
        _ <- refreshQueues config store pendingPQ suspendedPQ toRevisitQ
        maxWeightedOrderM <- getMaxWeightedOrder' pendingPQ suspendedPQ
        case maxWeightedOrderM of
          Just (WeightedOrderWithTimestamp oId _ _) -> get oId <&> fmap backlogOrder
          Nothing -> pure Nothing
    , drop = \oId -> do
        _ <- modifyIORef pendingPQ   (PQ.filter (\(WeightedOrderWithTimestamp orderId _ _) -> orderId /= oId))
        _ <- modifyIORef suspendedPQ (PQ.filter (\(WeightedOrderWithTimestamp orderId _ _) -> orderId /= oId))
        _ <- modifyIORef toRevisitQ  (Seq.filter (\(WeightedOrderWithTimestamp orderId _ _) -> orderId /= oId))
        drop oId
    }

refreshQueues
  :: (MonadIO m)
  => BacklogServiceConfig
  -> BacklogStore m
  -> IORef (PQ.MaxQueue WeightedOrderWithTimestamp)
  -> IORef (PQ.MaxQueue WeightedOrderWithTimestamp)
  -> IORef (Seq.Seq WeightedOrderWithTimestamp)
  -> m ()
refreshQueues BacklogServiceConfig{..} BacklogStore{..} pendingQueueRef suspendedQueueRef toRevisitSeqRef = do
  currentTime <- getCurrentTime
  revisited2process <-
    atomicModifyIORef'
      toRevisitSeqRef
      (Seq.partition (\(WeightedOrderWithTimestamp _ _ oTime) -> diffUTCTime currentTime oTime < orderExecTime))
  suspended2drop   <-
    atomicModifyIORef'
      suspendedQueueRef
      (PQ.partition (\(WeightedOrderWithTimestamp _ _ oTime) -> diffUTCTime currentTime oTime < orderLifetime))
  _ <- (\order@(WeightedOrderWithTimestamp oId _ oTime) ->
          if diffUTCTime currentTime oTime < orderLifetime
            then modifyIORef' pendingQueueRef (PQ.insert order)
            else drop oId
       ) `traverse` revisited2process
  _ <- (\(WeightedOrderWithTimestamp oId _ _) -> drop oId) `traverse` PQ.toList suspended2drop
  pure ()

getMaxWeightedOrder'
  :: (MonadIO m)
  => IORef (PQ.MaxQueue WeightedOrderWithTimestamp)
  -> IORef (PQ.MaxQueue WeightedOrderWithTimestamp)
  -> m (Maybe WeightedOrderWithTimestamp)
getMaxWeightedOrder' pendingQueueRef suspendedQueueRef = do
  pendingQueue   <- readIORef pendingQueueRef
  suspendedQueue <- readIORef suspendedQueueRef
  let
    maxPendingM   = PQ.getMax pendingQueue
    maxSuspendedM = PQ.getMax suspendedQueue
  case (maxPendingM, maxSuspendedM) of
    (Just maxPending, Just maxSuspended) ->
      if maxPending > maxSuspended
        then modifyIORef suspendedQueueRef (PQ.insert maxSuspended) >> pure maxPendingM
        else modifyIORef pendingQueueRef (PQ.insert maxPending) >> pure maxSuspendedM
    (Just _, Nothing) -> pure maxPendingM
    (Nothing, Just _) -> pure maxSuspendedM
    _ -> pure Nothing

recover
  :: (MonadIO m)
  => BacklogStore m
  -> BacklogServiceConfig
  -> IORef (PQ.MaxQueue WeightedOrderWithTimestamp)
  -> m ()
recover BacklogStore{..} BacklogServiceConfig{..} pendingQueueRef = do
  ordersInDb  <- getAll
  currentTime <- getCurrentTime
  let
    filteredOrders = List.filter (\BacklogOrder{..} -> diffUTCTime currentTime orderTimestamp < orderExecTime) ordersInDb
  modifyIORef 
    pendingQueueRef
    (\queue -> foldr (\BacklogOrder{..} -> PQ.insert (mkWeightedOrderWithTimestamp backlogOrder orderTimestamp)) queue filteredOrders)

attachLogging :: Monad m => Logging m -> BacklogService m -> BacklogService m
attachLogging Logging{..} BacklogService{..}=
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
        infoM @String "tryAcquire"
        r <- tryAcquire
        infoM $ "tryAcquire -> " <> show r
        pure r
    , drop = \orderId -> do
        infoM $ "drop " <> show orderId
        r <- drop orderId
        infoM $ "drop " <> show orderId <> " -> " <> show r
        pure r
    }
