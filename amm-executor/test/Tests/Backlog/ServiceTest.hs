{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
module Tests.Backlog.ServiceTest where

import Test.Tasty
import Test.Tasty.HUnit
import Hedgehog
import Spectrum.Executor.Backlog.Service (BacklogService (BacklogService, put, tryAcquire, checkLater), mkBacklogService)
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

instance LiftK IO IO where
  liftK = id


checkBacklogService = testGroup "CheckBacklogService"
  [ HH.testProperty "try_acquire_of_pending_orders_is_correct" getOrderTest
  , HH.testProperty "drop_outdated_orders" dropAllOutdatedOrders
  ]

getOrderTest :: Property
getOrderTest = property $ do
  pkh                <- forAll genPubKeyHash
  pool               <- forAll genPool
  swapOrders         <- forAll $ Gen.list (Range.linear 0 100) (genSwapOrder pkh pool)
  BacklogService{..} <- liftIO $ mkService cfgForOnlyPendingOrders
  currentTime        <- getCurrentTime
  let pendingOrders = swapOrders <&> (`PendingOrder` currentTime)
  liftIO $ put `traverse` pendingOrders
  inState <- liftIO $ const tryAcquire `traverse` pendingOrders
  let filtered = filter isJust inState
  length filtered === length pendingOrders

dropAllOutdatedOrders :: Property
dropAllOutdatedOrders = property $ do
  pkh                <- forAll genPubKeyHash
  pool               <- forAll genPool
  swapOrders         <- forAll $ Gen.list (Range.linear 0 100) (genSwapOrder pkh pool)
  BacklogService{..} <- liftIO $ mkService cfgForOnlyPendingOrders
  currentTime        <- getCurrentTime
  let
    hundredMinutes = -100 * 60
    inProgressOrders = swapOrders <&> (`InProgressOrder` addUTCTime hundredMinutes currentTime)
  liftIO $ checkLater `traverse` inProgressOrders
  inState <- liftIO $ const tryAcquire `traverse` inProgressOrders
  let filtered = filter isJust inState
  length filtered === 0

mkService :: forall f . (MonadIO f, MonadIO f, LiftK f f) => BacklogServiceConfig -> f (BacklogService f)
mkService cfg = do
  store <- mkMockStorage
  mkBacklogService mkMakeLogging cfg store

cfgForOnlyPendingOrders :: BacklogServiceConfig
cfgForOnlyPendingOrders = BacklogServiceConfig
  { orderLifetime = 600 :: NominalDiffTime
  , orderExecTime = 900 :: NominalDiffTime
  , suspendedPropability = -1
  }