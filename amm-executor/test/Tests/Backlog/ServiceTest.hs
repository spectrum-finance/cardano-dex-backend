{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Tests.Backlog.ServiceTest where

import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
  ( testGroup )
import Hedgehog
  ( (===), forAll, property, Property )
import Control.Monad.Trans.Resource
  ( MonadResource )
import RIO.Time
  (secondsToDiffTime, NominalDiffTime, getCurrentTime, addUTCTime)
import RIO
  ( MonadIO(..), fromMaybe, isJust, (<&>) )
import RIO.List
  ( headMaybe )
import Test.Tasty.Hedgehog
  ( testProperty )
import qualified Data.List as List

import qualified Plutus.Script.Utils.V2.Address  as PV2

import ErgoDex.PValidators
  ( swapValidator )

import Spectrum.Executor.Types
  ( weightOrder, OrderWeight, OrderWithCreationTime (OrderWithCreationTime) )
import Spectrum.Executor.Backlog.Service
  ( BacklogService (BacklogService, tryAcquire, checkLater, put), mkBacklogService' )
import Spectrum.Executor.Backlog.Config
  ( BacklogServiceConfig (orderLifetime, suspendedPropability, orderExecTime, BacklogServiceConfig) )
import Spectrum.Prelude.HigherKind
  ( LiftK(liftK) )
import Spectrum.Executor.Data.OrderState
  ( OrderInState(PendingOrder, InProgressOrder) )
import Spectrum.Executor.Backlog.Persistence.BacklogStore
  ( BacklogStore (BacklogStore, put, getAll) )
import Spectrum.Executor.Backlog.Data.BacklogOrder
  ( BacklogOrder(BacklogOrder) )
import Gen.OrdersGen
  ( genSwapOrder, genPool, genPubKeyHash )
import Gen.LoggingGen
  ( mkMakeLogging )
import Tests.Backlog.PersistenceMock
  ( mkMockStorage )
import Gen.ConstantsGen (cfgForOnlyPendingOrders)
import Gen.ServicesGen (mkService, mkBacklogService)

checkBacklogService = testGroup "CheckBacklogService"
  [ testProperty "retry_non_executed_Orders"                       retryNonExecutedOrders
  , testProperty "drop_outdated_orders"                            dropAllOutdatedOrders
  , testProperty "correct_pending_orders_order_during_try_acquire" getOrderTest
  ]

getOrderTest :: Property
getOrderTest = property $ do
  pool                            <- forAll genPool
  swapInstance                    <- swapValidator
  swapOrders                      <- forAll $ Gen.list (Range.linear 1 100) (genSwapOrder swapInstance pool)
  BacklogService{put, tryAcquire} <- liftIO mkBacklogService
  currentTime                     <- getCurrentTime
  let pendingOrders = swapOrders <&> (`PendingOrder` currentTime)
  liftIO $ put `traverse` pendingOrders
  inState <- liftIO $ const (tryAcquire <&> fmap (\(OrderWithCreationTime order _) -> order)) `traverse` pendingOrders
  let
    filteredOrders     = filter isJust inState
    filteredOrdersHead = fromMaybe Nothing (headMaybe filteredOrders) <&> weightOrder
    (isSorted, _)      = List.foldl (\ (acc, prevOrderWeightM) orderM ->
      case (prevOrderWeightM, orderM) of
        (Just prevOrder, Just order) ->
          let orderWeight = weightOrder order
          in (prevOrder >= orderWeight, Just orderWeight)
        _ -> (False, Nothing)
     ) (True, filteredOrdersHead) (tail filteredOrders)
  length filteredOrders === length pendingOrders
  isSorted              === True

dropAllOutdatedOrders :: Property
dropAllOutdatedOrders = property $ do
  pool                                    <- forAll genPool
  swapInstance                            <- swapValidator
  swapOrders                              <- forAll $ Gen.list (Range.linear 1 100) (genSwapOrder swapInstance pool)
  store@BacklogStore{put, getAll}         <- mkMockStorage
  BacklogService{checkLater, tryAcquire}  <- liftIO mkBacklogService
  currentTime                             <- getCurrentTime
  let
    hundredMinutes   = 100 * 60
    inProgressOrders = swapOrders <&> (`InProgressOrder` addUTCTime (-hundredMinutes) currentTime)
  liftIO $ (\(InProgressOrder order timestamp) -> put (BacklogOrder timestamp order)) `traverse` inProgressOrders
  liftIO $ checkLater `traverse` inProgressOrders
  inState   <- liftIO $ const tryAcquire `traverse` inProgressOrders
  inStorage <- liftIO getAll
  let filtered = filter isJust inState
  length filtered  === 0
  length inStorage === 0

retryNonExecutedOrders :: Property
retryNonExecutedOrders = property $ do
  pool                                   <- forAll genPool
  swapInstance                           <- swapValidator
  swapOrders                             <- forAll $ Gen.list (Range.linear 1 100) (genSwapOrder swapInstance pool)
  store@BacklogStore{put, getAll}        <- mkMockStorage
  BacklogService{checkLater, tryAcquire} <- liftIO mkBacklogService
  currentTime                            <- getCurrentTime
  let
    elevenMinutes    = 11 * 60
    inProgressOrders = swapOrders <&> (`InProgressOrder` addUTCTime (-elevenMinutes) currentTime)
  liftIO $ (\(InProgressOrder order timestamp) -> put (BacklogOrder timestamp order)) `traverse` inProgressOrders
  liftIO $ checkLater `traverse` inProgressOrders
  inState <- liftIO $ const tryAcquire `traverse` inProgressOrders
  let filtered = filter isJust inState
  length filtered === length inProgressOrders
