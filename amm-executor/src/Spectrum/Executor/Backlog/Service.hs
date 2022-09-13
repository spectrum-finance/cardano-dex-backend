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
  ( (<&>), atomicModifyIORef, modifyIORef', newIORef, IORef, MonadReader )
import RIO.Time 
  ( getCurrentTime, diffUTCTime )

import System.Logging.Hlog 
  ( MakeLogging(MakeLogging, forComponent), Logging (Logging, infoM) )

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
  ( Order, OrderId )
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
  , tryAcquire :: m (Maybe Order)
  , drop       :: OrderId -> m ()
  }

mkBacklogService
  :: forall f m env.
    ( MonadIO f
    , MonadResource f
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
  logging     <- forComponent "BacklogService"
  -- those queues should be shared with Backlog.Proceess (to make live updates). So maybe worth extracting them into separate module. e.g. BacklogStore
  pendingPQ   <- newIORef PQ.empty  -- ordered by weight; new orders
  suspendedPQ <- newIORef PQ.empty  -- ordered by weight; failed orders, waiting for retry (retries are performed with some constant probability, e.g. 5%) 
  toRevisitQ  <- newIORef Seq.empty -- regular queue; successully submitted orders. Left orders should be re-executed in X minutes. Normally successfully confirmed orders are eliminated from this queue.
  _           <- liftK $ recover logging store config pendingPQ
  pure $ attachLogging logging BacklogService
    { put = \(PendingOrder order timestamp) -> do
        put $ BacklogOrder timestamp order
        modifyIORef' pendingPQ (PQ.insert $ mkWeightedOrder order timestamp)
    , suspend = \(SuspendedOrder order timestamp) -> do
        modifyIORef' suspendedPQ (PQ.insert $ mkWeightedOrder order timestamp)
        exists $ BacklogOrder timestamp order
    , checkLater = \(InProgressOrder order timestamp) -> do
        modifyIORef' toRevisitQ (mkWeightedOrder order timestamp Seq.<|)
        exists $ BacklogOrder timestamp order
    , tryAcquire = do
        revisitOrders config store pendingPQ toRevisitQ
        getMaxWeightedOrder' config store pendingPQ suspendedPQ
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
    atomicModifyIORef
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
  -> IORef (PQ.MaxQueue WeightedOrder)
  -> IORef (PQ.MaxQueue WeightedOrder)
  -> m (Maybe Order)
getMaxWeightedOrder' cfg@BacklogServiceConfig{..} store pendingQueueRef suspendedQueueRef = do
  randomInt <- randomRIO (0, 100) :: m Int
  if randomInt > naturalToInt suspendedPropability
    then getMaxPendingOrder cfg store pendingQueueRef
    else getMaxSuspendedOrder cfg store suspendedQueueRef

getMaxPendingOrder
  :: MonadIO m
  => BacklogServiceConfig
  -> BacklogStore m
  -> IORef (PQ.MaxQueue WeightedOrder)
  -> m (Maybe Order)
getMaxPendingOrder cfg@BacklogServiceConfig{..} store@BacklogStore{..} pendingQueueRef = do
  currentTime <- getCurrentTime
  wOrderM     <- atomicModifyIORef pendingQueueRef (\queue -> case PQ.maxView queue of
      Just (order, newQueue) -> (newQueue, Just order)
      Nothing -> (queue, Nothing)
    )
  case wOrderM of 
    Just (WeightedOrder oId _ oTime) -> do
      if diffUTCTime currentTime oTime > orderLifetime
      then dropOrder oId >> getMaxPendingOrder cfg store pendingQueueRef
      else get oId <&> fmap backlogOrder
    Nothing -> pure Nothing
  
getMaxSuspendedOrder
  :: (MonadIO m)
  => BacklogServiceConfig
  -> BacklogStore m
  -> IORef (PQ.MaxQueue WeightedOrder)
  -> m (Maybe Order)
getMaxSuspendedOrder cfg@BacklogServiceConfig{..} store@BacklogStore{..} suspendedQueueRef = do
  currentTime   <- getCurrentTime
  maxSuspendedM <- atomicModifyIORef suspendedQueueRef (\queue -> case PQ.maxView queue of
      Just (order, newQueue) -> (newQueue, Just order)
      Nothing -> (queue, Nothing)
    )
  case maxSuspendedM of
    Just (WeightedOrder oId _ oTime) ->
      if diffUTCTime currentTime oTime > orderLifetime
      then dropOrder oId >> getMaxSuspendedOrder cfg store suspendedQueueRef
      else get oId <&> fmap backlogOrder
    Nothing -> pure Nothing

recover
  :: (MonadIO m)
  => Logging m 
  -> BacklogStore m
  -> BacklogServiceConfig
  -> IORef (PQ.MaxQueue WeightedOrder)
  -> m ()
recover Logging{..} BacklogStore{..} BacklogServiceConfig{..} pendingQueueRef = do
  ordersInDb  <- getAll
  _           <- infoM ("Orders in db length: " ++ show (length ordersInDb))
  currentTime <- getCurrentTime
  _           <- infoM ("Current time" ++ show currentTime)
  let
    filteredOrders = List.filter (\BacklogOrder{..} -> diffUTCTime currentTime orderTimestamp < orderExecTime) ordersInDb
  _           <- infoM ("Filtered orders length: " ++ show (length filteredOrders))
  modifyIORef'
    pendingQueueRef
    (\queue -> foldr (\BacklogOrder{..} -> PQ.insert (mkWeightedOrder backlogOrder orderTimestamp)) queue filteredOrders)

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
