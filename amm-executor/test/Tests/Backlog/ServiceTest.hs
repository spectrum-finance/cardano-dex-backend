{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tests.Backlog.ServiceTest where

import Test.Tasty
import Test.Tasty.HUnit
import Hedgehog
import Spectrum.Executor.Backlog.Service (BacklogService (BacklogService, tryAcquire, checkLater, put), mkBacklogService)
import Gen.LoggingGen (mkMakeLogging)
import Tests.Backlog.PersistenceMock (mkMockStorage)
import Control.Monad.Trans.Resource ( MonadResource )
import Spectrum.Executor.Backlog.Config (BacklogServiceConfig (orderLifetime, suspendedPropability, orderExecTime, BacklogServiceConfig))
import RIO.Time (secondsToDiffTime, NominalDiffTime, getCurrentTime, addUTCTime)
import Spectrum.HigherKind (LiftK(liftK))
import RIO
import Gen.OrdersGen (genSwapOrder, genPool, genPubKeyHash)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Spectrum.Executor.Data.OrderState (OrderInState(PendingOrder, InProgressOrder))
import Test.Tasty.Hedgehog as HH
import Spectrum.Executor.Backlog.Persistence.BacklogStore (BacklogStore (BacklogStore, put, getAll))
import Spectrum.Executor.Backlog.Data.BacklogOrder
import ErgoDex.Amm.Orders
import qualified Data.List as List
import Spectrum.Executor.Types (weightOrder, OrderWeight)
import RIO.List
  ( headMaybe )

instance LiftK IO IO where
  liftK = id

checkBacklogService = testGroup "CheckBacklogService"
  [ HH.testProperty "retry_non_executed_Orders" retryNonExecutedOrders
  , HH.testProperty "drop_outdated_orders" dropAllOutdatedOrders
  , HH.testProperty "correct_pending_orders_order_during_try_acquire" dropAllOutdatedOrders
  ]

getOrderTest :: Property
getOrderTest = property $ do
  pkh                <- forAll genPubKeyHash
  pool               <- forAll genPool
  swapOrders         <- forAll $ Gen.list (Range.linear 0 100) (genSwapOrder pkh pool)
  store              <- mkMockStorage
  BacklogService{put, tryAcquire} <- liftIO $ mkService cfgForOnlyPendingOrders store
  currentTime        <- getCurrentTime
  let pendingOrders = swapOrders <&> (`PendingOrder` currentTime)
  liftIO $ put `traverse` pendingOrders
  inState <- liftIO $ const tryAcquire `traverse` pendingOrders
  let
    filteredOrders = filter isJust inState
    filteredOrdersHead = fromMaybe Nothing (headMaybe filteredOrders) <&> weightOrder
    (isSorted, _) = List.foldl (\ (acc, prevOrderWeightM) orderM ->
      case (prevOrderWeightM, orderM) of
        (Just prevOrder, Just order) -> 
          let orderWeight = weightOrder order
          in (prevOrder > orderWeight, Just orderWeight)
        _ -> (False, Nothing)
     ) (True, filteredOrdersHead) (tail filteredOrders)
  length filteredOrders === length pendingOrders
  isSorted === True

dropAllOutdatedOrders :: Property
dropAllOutdatedOrders = property $ do
  pkh                 <- forAll genPubKeyHash
  pool                <- forAll genPool
  swapOrders          <- forAll $ Gen.list (Range.linear 0 100) (genSwapOrder pkh pool)
  store@BacklogStore{put, getAll}         <- mkMockStorage
  BacklogService{checkLater, tryAcquire}  <- liftIO $ mkService cfgForOnlyPendingOrders store
  currentTime         <- getCurrentTime
  let
    hundredMinutes = -100 * 60
    inProgressOrders = swapOrders <&> (`InProgressOrder` addUTCTime hundredMinutes currentTime)
  liftIO $ (\(InProgressOrder order timestamp) -> put (BacklogOrder timestamp order)) `traverse` inProgressOrders
  liftIO $ checkLater `traverse` inProgressOrders
  inState <- liftIO $ const tryAcquire `traverse` inProgressOrders
  inStorage <- liftIO getAll
  let filtered = filter isJust inState
  length filtered === 0
  length inStorage === 0

retryNonExecutedOrders :: Property
retryNonExecutedOrders = property $ do
  pkh                <- forAll genPubKeyHash
  pool               <- forAll genPool
  swapOrders         <- forAll $ Gen.list (Range.linear 0 100) (genSwapOrder pkh pool)
  store@BacklogStore{put, getAll}              <- mkMockStorage
  BacklogService{checkLater, tryAcquire} <- liftIO $ mkService cfgForOnlyPendingOrders store
  currentTime        <- getCurrentTime
  let
    elevenMinutes = -11 * 60
    inProgressOrders = swapOrders <&> (`InProgressOrder` addUTCTime elevenMinutes currentTime)
  liftIO $ (\(InProgressOrder order timestamp) -> put (BacklogOrder timestamp order)) `traverse` inProgressOrders
  liftIO $ checkLater `traverse` inProgressOrders
  inState <- liftIO $ const tryAcquire `traverse` inProgressOrders
  let filtered = filter isJust inState
  length filtered === length inProgressOrders

mkService :: forall f . (MonadIO f, MonadIO f, LiftK f f) => BacklogServiceConfig -> BacklogStore f -> f (BacklogService f)
mkService = mkBacklogService mkMakeLogging

cfgForOnlyPendingOrders :: BacklogServiceConfig
cfgForOnlyPendingOrders = BacklogServiceConfig
  { orderLifetime = 900 :: NominalDiffTime
  , orderExecTime = 600 :: NominalDiffTime
  , suspendedPropability = -1
  }