{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Spectrum.Executor.OrdersExecutor.Process
  ( OrdersExecutor(..)
  , mkOrdersExecutor
  ) where

import Prelude hiding (drop)
import RIO.Time
  ( UTCTime, getCurrentTime )
import RIO
  ( (&), MonadReader, MonadUnliftIO, MonadIO (liftIO), QSem, waitQSem, (<&>) )
import qualified RIO.List as List
import Streamly.Prelude as S
  ( repeatM, mapM, MonadAsync, IsStream, before )
import Control.Monad.Catch
  ( MonadThrow, SomeException, MonadCatch, catches, Handler (Handler) )

import System.Logging.Hlog
  ( MakeLogging (MakeLogging, forComponent), Logging (Logging, infoM) )

import CardanoTx.Models
  ( TxCandidate, fullTxOutDatum, fullTxOutRef, FullTxOut (FullTxOut) )
import qualified CardanoTx.Interop as Interop
import Cardano.Api
  ( Tx )
import Ouroboros.Network.Subscription.PeerState ()

import qualified ErgoDex.Amm.Orders as Core
import qualified ErgoDex.Amm.Pool   as Core
import ErgoDex.Amm.Orders
  ( OrderAction (DepositAction, RedeemAction, SwapAction) )
import ErgoDex.State
  ( OnChain(OnChain), Predicted (Predicted) )
import ErgoDex.Amm.PoolActions
  ( PoolActions (PoolActions, runDeposit, runRedeem, runSwap)
  , OrderExecErr (EmptyPool, PoolNotFoundInFinalTx, PriceTooHigh)
  )
import SubmitAPI.Service
  ( Transactions(..) )
import Spectrum.Prelude.Throw
  ( throwMaybe, throwEither )

import Spectrum.Executor.Backlog.Service
  ( BacklogService (BacklogService, suspend, drop, tryAcquire, checkLater) )
import Spectrum.Executor.Types
  ( Order, Pool, orderId, OrderWithCreationTime (OrderWithCreationTime) )
import Spectrum.Prelude.Context
  ( HasType, askContext )
import Spectrum.Executor.Config
  ( TxRefs (..) )
import Spectrum.Executor.PoolTracker.Service
  ( PoolResolver (PoolResolver, resolvePool, putPool) )
import Spectrum.Executor.PoolTracker.Data.Traced
  ( Traced(Traced, prevTxOutRef, tracedState) )
import Spectrum.Executor.Data.OrderState
  ( OrderInState(InProgressOrder, SuspendedOrder) )
import Explorer.Service
  ( Explorer (Explorer, getOutput), getOutput )
import Data.Maybe
  ( catMaybes )
import Explorer.Class
  ( toCardanoTx )

import qualified Ledger.Tx.CardanoAPI as Interop
import qualified Spectrum.Executor.Data.State as State
import RIO.Text (isInfixOf)
import Data.Text (pack)
import Control.Concurrent (threadDelay)
import Data.Aeson (encode)

newtype OrdersExecutor s m = OrdersExecutor
  { run :: s m ()
  }

mkOrdersExecutor
  :: forall f m s env era.
    ( IsStream s
    , MonadAsync m
    , MonadUnliftIO m
    , MonadReader env f
    , HasType (MakeLogging f m) env
    , HasType TxRefs env, MonadCatch m)
  => BacklogService m
  -> QSem
  -> Transactions m era
  -> Explorer m
  -> PoolResolver m
  -> PoolActions
  -> f (OrdersExecutor s m)
mkOrdersExecutor backlog syncSem transactions explorer resolver poolActions = do
  MakeLogging{..} <- askContext
  txRefsCfg       <- askContext
  logging         <- forComponent "OrdersExecutor"
  pure $ OrdersExecutor
    { run = run' logging syncSem txRefsCfg backlog transactions explorer resolver poolActions
    }

run'
  :: forall s m era. (IsStream s, MonadAsync m, MonadUnliftIO m, MonadCatch m)
  => Logging m
  -> QSem
  -> TxRefs
  -> BacklogService m
  -> Transactions m era
  -> Explorer m
  -> PoolResolver m
  -> PoolActions
  -> s m ()
run' logging@Logging{..} syncSem txRefs backlog@BacklogService{..} txs explorer resolver poolActions =
  S.before (liftIO $ waitQSem syncSem) $ S.repeatM tryAcquire & S.mapM (\case
      Just orderWithCreationTime ->
        infoM ("Going to execute order for pool" ++ show orderWithCreationTime) >>
          execute' logging txRefs backlog txs explorer resolver poolActions orderWithCreationTime
      Nothing    ->
        pure ()
    )

execute'
  :: forall m era. (MonadUnliftIO m, MonadCatch m)
  => Logging m
  -> TxRefs
  -> BacklogService m
  -> Transactions m era
  -> Explorer m
  -> PoolResolver m
  -> PoolActions
  -> OrderWithCreationTime
  -> m ()
execute' l@Logging{..} txRefs backlog@BacklogService{suspend, drop} txs explorer resolver poolActions (OrderWithCreationTime order orderTime) = do
  executionStartTime <- getCurrentTime
  executeOrder' backlog l txRefs txs explorer resolver poolActions order executionStartTime `catches`
    [ Handler (\ (execErr :: OrderExecErr) -> case execErr of
        PriceTooHigh ->
          suspend (SuspendedOrder order orderTime) >> infoM ("Err PriceTooHigh occured for " ++ show order ++ ". Going to suspend")
        dropError ->
          drop (orderId order) >> infoM ("Err " ++ show dropError ++ " occured for " ++ show order ++ ". Going to drop")
      )
    , Handler (\ (dropError :: SomeException) -> processOrderExecutionException l backlog dropError order orderTime)
    ]

processOrderExecutionException :: Monad m => Logging m -> BacklogService m -> SomeException -> Order -> UTCTime -> m ()
processOrderExecutionException Logging{..} BacklogService{suspend, drop} executionError order@(OnChain FullTxOut{..} _) orderTime =
  let
    orderTxId  = Interop.toCardanoTxIn fullTxOutRef
    errMsgText = pack (show executionError)
  in if isInfixOf "BadInputsUTxO" errMsgText && not (pack (show orderTxId) `isInfixOf` errMsgText)
       then
         suspend (SuspendedOrder order orderTime) >>
           infoM ("Got BadInputsUTxO error during order (" ++ show order ++ ") execution without orderId. " ++ show errMsgText ++ ". Going to suspend order")
       else
         drop (orderId order) >>
           infoM ("Got error during order (" ++ show order ++ ") " ++ show errMsgText ++ ". Going to drop order")
           
executeOrder'
  :: (Monad m, MonadThrow m)
  => BacklogService m
  -> Logging m
  -> TxRefs
  -> Transactions m era
  -> Explorer m
  -> PoolResolver m
  -> PoolActions
  -> Order
  -> UTCTime
  -> m ()
executeOrder'
  BacklogService{checkLater}
  Logging{..}
  txRefs
  Transactions{..}
  explorer
  PoolResolver{..}
  poolActions
  order@(OnChain _ Core.AnyOrder{..})
  executionStartTime = do
    mPool <- resolvePool anyOrderPoolId

    pool@(OnChain prevPoolOut Core.Pool{poolId}) <- throwMaybe (EmptyPool anyOrderPoolId) mPool
    (txCandidate, Predicted _ predictedPool)     <- runOrder txRefs explorer pool order poolActions
    infoM ("txCandidate: " ++ show (encode txCandidate))
    tx    <- finalizeTx txCandidate
    pPool <- throwMaybe (PoolNotFoundInFinalTx poolId) (extractPoolTxOut pool tx)
    let
      tracedPredictedPool = Traced
        { tracedState  = State.Predicted (OnChain pPool predictedPool)
        , prevTxOutRef = fullTxOutRef prevPoolOut
        }
    _ <- submitTx tx
    putPool tracedPredictedPool
    _ <- checkLater (InProgressOrder order executionStartTime)
    pure ()

extractPoolTxOut :: forall era. Pool -> Tx era -> Maybe FullTxOut
extractPoolTxOut (OnChain poolOutput _) tx =
  List.find (\output -> fullTxOutDatum output == fullTxOutDatum poolOutput) (Interop.extractCardanoTxOutputs tx)

runOrder
  :: (MonadThrow m)
  => TxRefs
  -> Explorer m
  -> Pool
  -> Order
  -> PoolActions
  -> m (TxCandidate, Predicted Core.Pool)
runOrder TxRefs{..} Explorer{..} (OnChain poolOut pool) (OnChain orderOut Core.AnyOrder{..}) PoolActions{..} = do
  let poolOutRef = Interop.fromCardanoTxIn poolRef
  poolRefOuput <- getOutput poolOutRef
  case anyOrderAction of
    DepositAction deposit -> do
      let depositOutRef = Interop.fromCardanoTxIn depositRef
      depositRefOut <- getOutput depositOutRef
      let refInputs = catMaybes [poolRefOuput, depositRefOut] <&> toCardanoTx
      throwEither $ runDeposit refInputs (OnChain orderOut deposit) (poolOut, pool)
    RedeemAction redeem   -> do
      let redeemOutRef = Interop.fromCardanoTxIn redeemRef
      redeemRefOut <- getOutput redeemOutRef
      let refInputs = catMaybes [poolRefOuput, redeemRefOut] <&> toCardanoTx
      throwEither $ runRedeem refInputs (OnChain orderOut redeem) (poolOut, pool)
    SwapAction swap       -> do
      let swapOutRef = Interop.fromCardanoTxIn swapRef
      swapRefOut <- getOutput swapOutRef
      let refInputs = catMaybes [poolRefOuput, swapRefOut] <&> toCardanoTx
      throwEither $ runSwap refInputs (OnChain orderOut swap) (poolOut, pool)
