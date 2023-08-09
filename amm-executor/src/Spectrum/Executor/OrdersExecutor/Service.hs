module Spectrum.Executor.OrdersExecutor.Service
  ( OrdersExecutorService(..)
  , mkOrdersExecutorService
  ) where

import Prelude hiding (drop)
import RIO.Time
  ( UTCTime, getCurrentTime, diffUTCTime, nominalDiffTimeToSeconds )
import RIO
  ( MonadReader, MonadUnliftIO, (<&>), MonadIO, Text )
import qualified RIO.List as List
import Control.Monad.Catch
  ( MonadThrow, SomeException, MonadCatch, catches, Handler (Handler) )

import System.Logging.Hlog
  ( MakeLogging (MakeLogging, forComponent), Logging (Logging, debugM, debugM) )
import CardanoTx.Models
  ( TxCandidate, fullTxOutDatum, fullTxOutRef, FullTxOut (FullTxOut) )
import qualified CardanoTx.Interop as Interop
import Cardano.Api
  ( Tx )
import Ouroboros.Network.Subscription.PeerState ()
import Ledger
  ( TxOutRef(txOutRefId) )

import qualified ErgoDex.Amm.Orders as Core
import qualified ErgoDex.Amm.Pool   as Core
import ErgoDex.Amm.Orders
  ( OrderAction (DepositAction, RedeemAction, SwapAction) )
import ErgoDex.State
  ( OnChain(OnChain), Predicted (Predicted) )
import ErgoDex.Amm.PoolActions
  ( PoolActions (PoolActions, runDeposit, runRedeem, runSwap, runDepositWithDebug, runRedeemWithDebug, runSwapWithDebug)
  , OrderExecErr (EmptyPool, PoolNotFoundInFinalTx, PriceTooHigh, InsufficientPoolLqForSwap)
  )
import SubmitAPI.Service
  ( Transactions(..) )
import Spectrum.Prelude.Throw
  ( throwMaybe, throwEither )

import Spectrum.Executor.Backlog.Service
  ( BacklogService (BacklogService, suspend, drop, checkLater) )
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
import Data.Aeson (encode)
import qualified System.Logging.Hlog as Trace
import Spectrum.Executor.OrdersExecutor.RefInputs (RefInputs(..))

data OrdersExecutorService m = OrdersExecutorService
  { execute :: OrderWithCreationTime -> m ()
  , executeUnsafe :: OrderWithCreationTime -> m ()
  }

mkOrdersExecutorService
  :: forall f m env era.
    ( MonadUnliftIO m
    , MonadReader env f
    , HasType (MakeLogging f m) env
    , HasType TxRefs env, MonadCatch m)
  => BacklogService m
  -> Transactions m era
  -> Explorer m
  -> PoolResolver m
  -> PoolActions
  -> RefInputs
  -> f (OrdersExecutorService m)
mkOrdersExecutorService backlog transactions explorer resolver poolActions refInputs = do
  MakeLogging{..} <- askContext
  txRefsCfg       <- askContext
  logging         <- forComponent "Bots.OrdersExecutorService"
  pure $ OrdersExecutorService
    { execute = execute' logging txRefsCfg backlog transactions explorer resolver poolActions
    , executeUnsafe = executeUnsafe' logging refInputs backlog transactions explorer resolver poolActions
    }

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
          suspend (SuspendedOrder order orderTime) >> infoM ("Err PriceTooHigh occured for " ++ show (orderId order) ++ ". Going to suspend")
        (InsufficientPoolLqForSwap poolId) ->
          suspend (SuspendedOrder order orderTime) >> infoM ("Err InsufficientPoolLqForSwap occured for " ++ show (orderId order) ++ " and pool " ++ show poolId ++  ". Going to suspend")
        dropError ->
          drop (orderId order) >> infoM ("Err " ++ show dropError ++ " occured for " ++ show (orderId order) ++ ". Going to drop")
      )
    , Handler (\ (dropError :: SomeException) -> processOrderExecutionException l backlog dropError order orderTime)
    ]
  executionEndTime <- getCurrentTime
  let timeDiff = fromEnum $ nominalDiffTimeToSeconds $ diffUTCTime executionEndTime executionStartTime
  infoM $ "Time of start order processing is " ++ show executionStartTime
  infoM $ "Time of end order processing is " ++ show executionEndTime
  infoM $ "Time of processing order is " ++ show (timeDiff `div` 1000000000) ++ " mills"

executeUnsafe'
  :: forall m era. (MonadUnliftIO m, MonadCatch m)
  => Logging m
  -> RefInputs
  -> BacklogService m
  -> Transactions m era
  -> Explorer m
  -> PoolResolver m
  -> PoolActions
  -> OrderWithCreationTime
  -> m ()
executeUnsafe' l@Logging{..} refInputs backlog@BacklogService{suspend, drop} txs explorer resolver poolActions (OrderWithCreationTime order orderTime) = do
  executionStartTime <- getCurrentTime
  executeOrderUnsafe' backlog l refInputs txs explorer resolver poolActions order executionStartTime `catches`
    [ Handler (\ (execErr :: OrderExecErr) -> case execErr of
        PriceTooHigh ->
          suspend (SuspendedOrder order orderTime) >> infoM ("(Unsafe) Err PriceTooHigh occured for " ++ show (orderId order) ++ ". Going to suspend")
        (InsufficientPoolLqForSwap poolId) ->
          suspend (SuspendedOrder order orderTime) >> infoM ("(Unsafe) Err InsufficientPoolLqForSwap occured for " ++ show (orderId order) ++ " and pool " ++ show poolId ++  ". Going to suspend")
        dropError ->
          drop (orderId order) >> infoM ("(Unsafe) Err " ++ show dropError ++ " occured for " ++ show (orderId order) ++ ". Going to drop")
      )
    , Handler (\ (dropError :: SomeException) -> processOrderExecutionException l backlog dropError order orderTime)
    ]
  executionEndTime <- getCurrentTime
  let timeDiff = fromEnum $ nominalDiffTimeToSeconds $ diffUTCTime executionEndTime executionStartTime
  infoM $ "(Unsafe) Time of start order processing is " ++ show executionStartTime
  infoM $ "(Unsafe) Time of end order processing is " ++ show executionEndTime
  infoM $ "(Unsafe) Time of processing order is " ++ show (timeDiff `div` 1000000000) ++ " mills"

processOrderExecutionException :: Monad m => Logging m -> BacklogService m -> SomeException -> Order -> UTCTime -> m ()
processOrderExecutionException Logging{..} BacklogService{suspend, drop} executionError order@(OnChain FullTxOut{..} _) orderTime = do
   let errMsgText = pack (show executionError)
   if isInfixOf "BadInputsUTxO" errMsgText && not (pack (show (txOutRefId fullTxOutRef)) `isInfixOf` errMsgText)
     then
       suspend (SuspendedOrder order orderTime) >>
         infoM ("Got BadInputsUTxO error during order (" ++ show (orderId order) ++ ") execution without orderId (" ++ show (txOutRefId fullTxOutRef) ++ "). " ++ show errMsgText ++ ". Going to suspend order")
     else drop (orderId order) >> infoM ("Got error (" ++ show (processDropErrorMsg errMsgText)++ ") during order (" ++ show (orderId order) ++ ") " ++ ". Going to drop order")

processDropErrorMsg :: Text -> Text
processDropErrorMsg errMsg
  | "The budget when the machine terminated was" `isInfixOf` errMsg = "Insufficient budget for order execution. Please increase uxUnits/exMem in unsafeEval config."
  | "BadInputsUTxO" `isInfixOf` errMsg = "Coudn't execute order, possibly another bot already execute it"
  | otherwise = pack ("Got unknown error: " ++ show errMsg ++ ". Please open ticket in our discord channel https://discord.com/invite/zY2gmTYQVD")

executeOrder'
  :: (MonadIO m, MonadThrow m)
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
  l@Logging{..}
  txRefs
  Transactions{..}
  explorer
  PoolResolver{..}
  poolActions
  order@(OnChain _ Core.AnyOrder{..})
  executionStartTime = do
    mPool <- resolvePool anyOrderPoolId
    pool@(OnChain prevPoolOut Core.Pool{poolId}) <- throwMaybe (EmptyPool anyOrderPoolId) mPool
    (txCandidate, Predicted _ predictedPool)     <- runOrder txRefs explorer pool order poolActions l
    tx    <- finalizeTx txCandidate
    pPool <- throwMaybe (PoolNotFoundInFinalTx poolId) (extractPoolTxOut pool tx)
    let
      tracedPredictedPool = Traced
        { tracedState  = State.Predicted (OnChain pPool predictedPool)
        , prevTxOutRef = fullTxOutRef prevPoolOut
        }
    putPool tracedPredictedPool
    _ <- checkLater (InProgressOrder order executionStartTime)
    pure ()

executeOrderUnsafe'
  :: (MonadIO m, MonadThrow m)
  => BacklogService m
  -> Logging m
  -> RefInputs
  -> Transactions m era
  -> Explorer m
  -> PoolResolver m
  -> PoolActions
  -> Order
  -> UTCTime
  -> m ()
executeOrderUnsafe'
  BacklogService{checkLater}
  l@Logging{..}
  refInputs
  Transactions{..}
  explorer
  PoolResolver{..}
  poolActions
  order@(OnChain _ Core.AnyOrder{..})
  executionStartTime = do
    mPool <- resolvePool anyOrderPoolId
    pool@(OnChain prevPoolOut Core.Pool{poolId}) <- throwMaybe (EmptyPool anyOrderPoolId) mPool
    (txCandidate, Predicted _ predictedPool, changeValue)   <- runOrderUnsafe refInputs explorer pool order poolActions l
    tx    <- finalizeTxUnsafe txCandidate changeValue
    _ <- submitTx tx
    pPool <- throwMaybe (PoolNotFoundInFinalTx poolId) (extractPoolTxOut pool tx)
    let
      tracedPredictedPool = Traced
        { tracedState  = State.Predicted (OnChain pPool predictedPool)
        , prevTxOutRef = fullTxOutRef prevPoolOut
        }
    infoM $ "tx: " ++ show tx
    putPool tracedPredictedPool
    _ <- checkLater (InProgressOrder order executionStartTime)
    pure ()

extractPoolTxOut :: forall era. Pool -> Tx era -> Maybe FullTxOut
extractPoolTxOut (OnChain poolOutput _) tx =
  List.find (\output -> fullTxOutDatum output == fullTxOutDatum poolOutput) (Interop.extractCardanoTxOutputs tx)

runOrderUnsafe
  :: ( MonadThrow m)
  => RefInputs
  -> Explorer m
  -> Pool
  -> Order
  -> PoolActions
  -> Logging m
  -> m (TxCandidate, Predicted Core.Pool, Integer)
runOrderUnsafe RefInputs{..} Explorer{..} (OnChain poolOut pool) (OnChain orderOut Core.AnyOrder{..}) PoolActions{..} Logging{..} = do
  case anyOrderAction of
    DepositAction deposit -> do
      throwEither (runDeposit [poolOutput, depositOutput] (OnChain orderOut deposit) (poolOut, pool))
    RedeemAction redeem   -> do
      throwEither $ runRedeem [poolOutput, redeemOutput] (OnChain orderOut redeem) (poolOut, pool)
    SwapAction swap       -> do
      throwEither $ runSwap [poolOutput, swapOutput] (OnChain orderOut swap) (poolOut, pool)

runOrder
  :: (Monad m, MonadThrow m)
  => TxRefs
  -> Explorer m
  -> Pool
  -> Order
  -> PoolActions
  -> Logging m
  -> m (TxCandidate, Predicted Core.Pool)
runOrder TxRefs{..} Explorer{..} (OnChain poolOut pool) (OnChain orderOut Core.AnyOrder{..}) PoolActions{..} Logging{..} = do
  let poolOutRef = Interop.fromCardanoTxIn poolRef
  poolRefOuput <- getOutput poolOutRef
  case anyOrderAction of
    DepositAction deposit -> do
      let depositOutRef = Interop.fromCardanoTxIn depositRef
      depositRefOut <- getOutput depositOutRef
      let
        refInputs = catMaybes [poolRefOuput, depositRefOut] <&> toCardanoTx
      case runDepositWithDebug refInputs (OnChain orderOut deposit) (poolOut, pool) of
        Left (err, orderInfo) ->
          infoM ("Order execution info" ++ show orderInfo) >> throwEither (Left err)
        Right (candidate, newPool, orderInfo) ->
          infoM ("Order execution info" ++ show orderInfo) >> pure (candidate, newPool)
    RedeemAction redeem   -> do
      let redeemOutRef = Interop.fromCardanoTxIn redeemRef
      redeemRefOut <- getOutput redeemOutRef
      let refInputs = catMaybes [poolRefOuput, redeemRefOut] <&> toCardanoTx
      case runRedeemWithDebug refInputs (OnChain orderOut redeem) (poolOut, pool) of
        Left (err, orderInfo) ->
          infoM ("Order execution info" ++ show orderInfo) >> throwEither (Left err)
        Right (candidate, newPool, orderInfo) ->
          infoM ("Order execution info" ++ show orderInfo) >> pure (candidate, newPool)
    SwapAction swap       -> do
      let swapOutRef = Interop.fromCardanoTxIn swapRef
      swapRefOut <- getOutput swapOutRef
      let refInputs = catMaybes [poolRefOuput, swapRefOut] <&> toCardanoTx
      case runSwapWithDebug refInputs (OnChain orderOut swap) (poolOut, pool) of
        Left (err, orderInfo) ->
          infoM ("Order execution info" ++ show orderInfo) >> throwEither (Left err)
        Right (candidate, newPool, orderInfo) ->
          infoM ("Order execution info" ++ show orderInfo) >> pure (candidate, newPool)