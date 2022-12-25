{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.OrdersExecutor.ProcessTest where

import RIO.Time 
  ( getCurrentTime )
import Streamly.Prelude as S
  ( drain, parallel )
import Hedgehog
  ( (===), forAll, property, Property )
import Test.Tasty
  ( testGroup )
import Test.Tasty.Hedgehog
  ( testProperty )
import Hedgehog.Internal.Property 
  ( withTests, TestLimit (..) )
import RIO
  ( MonadIO (..), (<&>), MonadTrans (lift) )
import Streamly.Internal.Control.ForkLifted 
  ( fork )
import System.Time.Extra
  ( sleep, Seconds )
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import WalletAPI.Vault 
  ( mkVault, Vault (..) )
import Ledger.Tx.CardanoAPI 
  ( fromCardanoPaymentKeyHash, toCardanoAddressInEra )
import ErgoDex.State 
  ( OnChain(..) )

import Spectrum.Executor.OrdersExecutor.Process
  ( OrdersExecutor(run) )
import Spectrum.Executor.Backlog.Service 
  ( BacklogService (..), mkBacklogService' )
import Spectrum.Executor.Data.OrderState 
  (OrderInState(..) )
import Spectrum.Executor.Backlog.Persistence.BacklogStore 
  ( BacklogStore(getAll) )

import Gen.LoggingGen 
  ( mkEmptyMakeLogging )
import Gen.OrdersGen 
  ( genSwapOrder, genRefMapForExplorer, genAdaFullTxOut, genOnChainPool )
import Gen.ServicesGen 
  ( mkCardanoNetworkMockWithSubmitionError
  , mkMockTrustStore
  , mkTestExplorer
  , mkMockOrdersExecutor
  , mkMockPoolResolver
  , mkService
  , mkTestVault
  )
import Gen.ConstantsGen 
  ( testTxRefs
  , mkValidators
  , cfgForOnlyPendingOrders
  , Validators (..)
  , ValidatorInfo (..)
  )
import Tests.Backlog.PersistenceMock 
  ( mkMockStorage )

checkOrdersExecutor = testGroup "Orders"
  [ testProperty "bad_inputs_utxo_test" processTest
  ]

defaultSleepTimeInSeconds :: Seconds
defaultSleepTimeInSeconds = 3

processTest :: Property
processTest = withTests 10 $ property $ do
  validators@Validators{..}    <- liftIO mkValidators
  onChainPool@(OnChain _ pool) <- forAll (genOnChainPool (validator poolInfo))

  mapForExplorer  <- forAll $ genRefMapForExplorer validators testTxRefs
  let
    vault        = mkTestVault
    poolResolver = mkMockPoolResolver onChainPool

  executorPkh           <- fmap fromCardanoPaymentKeyHash (getPaymentKeyHash vault)
  executorCollateralOut <- forAll $ genAdaFullTxOut executorPkh

  swapOrders     <- forAll $ Gen.list (Range.linear 1 10) (genSwapOrder (validator swapInfo) pool)
  currentTime    <- getCurrentTime
  store          <- mkMockStorage
  let
    pendingOrders = swapOrders <&> (`PendingOrder` currentTime)
    explorer      = mkTestExplorer executorCollateralOut mapForExplorer

  backlogService <- liftIO $ mkBacklogService' mkEmptyMakeLogging cfgForOnlyPendingOrders store
  executor       <- liftIO $ mkMockOrdersExecutor swapOrders backlogService poolResolver explorer

  liftIO $ put backlogService `traverse` pendingOrders
  lift . fork $ S.drain $ run executor

  liftIO $ sleep defaultSleepTimeInSeconds

  orders <- liftIO $ getAll store
  length orders === length swapOrders
