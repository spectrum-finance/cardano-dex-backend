module Spectrum.Executor.OrdersExecutor.Service
  ( OrdersExecutor(..)
  , mkOrdersExecutor
  ) where

import SubmitAPI.Service
import Spectrum.Executor.Backlog.Service (BacklogService (BacklogService, suspend, drop, tryAcquire, checkLater))
import Spectrum.Executor.Types
    ( Order(..),
      Pool,
      orderId )
import Spectrum.Context
  ( HasType, askContext )
import RIO
  ( (&), MonadReader, catch, MonadUnliftIO)
import qualified ErgoDex.Amm.Orders as Core
import qualified ErgoDex.Amm.Pool as Core
import System.Logging.Hlog (MakeLogging (MakeLogging, forComponent), Logging (Logging, infoM))
import ErgoDex.Amm.Orders (OrderAction (DepositAction, RedeemAction, SwapAction), AnyOrder (AnyOrder, anyOrderPoolId))
import ErgoDex.State (OnChain(OnChain), Predicted (Predicted), Confirmed (Confirmed))
import Spectrum.Executor.PoolTracker.Service (PoolResolver (PoolResolver, resolvePool, putPool))
import ErgoDex.Amm.PoolActions (PoolActions (PoolActions, runDeposit, runRedeem, runSwap), OrderExecErr (EmptyPool, PoolNotFoundInFinalTx, PriceTooHigh))
import CardanoTx.Models (TxCandidate, fullTxOutDatum, fullTxOutRef, FullTxOut)
import Core.Throw.Combinators (throwMaybe, throwEither)
import Control.Monad.Catch (MonadThrow)
import Streamly.Prelude as S
import qualified CardanoTx.Interop as Interop
import Cardano.Api (Tx)
import qualified RIO.List as List
import Spectrum.Executor.PoolTracker.Data.Traced (Traced(Traced, prevTxOutRef, tracedState))
import CardanoTx.ToPlutus (ToPlutus(toPlutus))
import Spectrum.Executor.Data.OrderState (OrderInState(InProgressOrder, SuspendedOrder))
import qualified Spectrum.Executor.Data.State as State
import Ouroboros.Network.Subscription.PeerState ()
import Prelude hiding (drop)
import RIO.Time (UTCTime, getCurrentTime)

data OrdersExecutor s m = OrdersExecutor
  { run :: s m ()
  }

mkOrdersExecutor
  :: forall f m s env era.
    ( IsStream s
    , MonadAsync m
    , MonadUnliftIO m
    , MonadReader env f
    , HasType (MakeLogging f m) env
    )
  => BacklogService m
  -> Transactions m era
  -> PoolResolver m
  -> PoolActions
  -> f (OrdersExecutor s m)
mkOrdersExecutor backlog transactions resolver poolActions = do
  MakeLogging{..} <- askContext
  logging         <- forComponent "OrdersExecutor"
  pure $ OrdersExecutor
    { run = run' logging backlog transactions resolver poolActions
    }

run'
  :: forall s m era. (IsStream s, MonadAsync m, MonadUnliftIO m)
  => Logging m
  -> BacklogService m
  -> Transactions m era
  -> PoolResolver m
  -> PoolActions
  -> s m ()
run' logging@Logging{..} backlog@BacklogService{..} txs resolver poolActions =
  S.repeatM tryAcquire & S.mapM (\case
      Just order ->
        infoM ("Going to execute order for pool" ++ show order) >> execute' logging backlog txs resolver poolActions order
      Nothing    ->
        pure ()
    )

execute' 
  :: forall m era. (MonadUnliftIO m, MonadThrow m) 
  => Logging m 
  -> BacklogService m 
  -> Transactions m era 
  -> PoolResolver m 
  -> PoolActions 
  -> Order 
  -> m ()
execute' Logging{..} backlog@BacklogService{suspend, drop} txs resolver poolActions order = do
  executionStartTime <- getCurrentTime
  catch (executeOrder' backlog txs resolver poolActions order executionStartTime) (\(err :: OrderExecErr) -> case err of
      PriceTooHigh  -> 
        suspend (SuspendedOrder order executionStartTime) >> infoM ("Price too high for order " ++ show order ++ ". Going to suspend")
      dropErr       -> 
        drop (orderId order) >> infoM ("Err " ++ show dropErr ++ " occured for " ++ show order ++ ". Going to drop")
    )

executeOrder' 
  :: (Monad m, MonadThrow m) 
  => BacklogService m 
  -> Transactions m era 
  -> PoolResolver m 
  -> PoolActions 
  -> Order 
  -> UTCTime
  -> m ()
executeOrder' BacklogService{checkLater} Transactions{..} PoolResolver{..} poolActions order@(OnChain _ Core.AnyOrder{..}) executionStartTime = do
  mPool <- resolvePool anyOrderPoolId
  pool@(OnChain prevPoolOut Core.Pool{poolId})  <- throwMaybe (EmptyPool anyOrderPoolId) mPool
  (txCandidate, Predicted _ predictedPool)      <- throwEither $ runOrder pool order poolActions
  tx    <- finalizeTx txCandidate
  pPool <- throwMaybe (PoolNotFoundInFinalTx poolId) (extractPoolTxOut pool tx)
  let
    tracedPredictedPool = Traced
      { tracedState  = State.Predicted (OnChain pPool predictedPool)
      , prevTxOutRef = fullTxOutRef prevPoolOut
      }
  submitTx tx
  putPool tracedPredictedPool
  checkLater (InProgressOrder order executionStartTime)
  pure ()

extractPoolTxOut :: forall era. Pool -> Tx era -> Maybe FullTxOut
extractPoolTxOut (OnChain poolOutput _) tx = 
  List.find (\output -> fullTxOutDatum output == fullTxOutDatum poolOutput) (Interop.extractCardanoTxOutputs tx)

runOrder
  :: Pool
  -> Order
  -> PoolActions
  -> Either OrderExecErr (TxCandidate, Predicted Core.Pool)
runOrder (OnChain poolOut pool) (OnChain orderOut Core.AnyOrder{..}) PoolActions{..} =
  case anyOrderAction of
    DepositAction deposit -> runDeposit (Confirmed orderOut deposit) (poolOut, pool)
    RedeemAction redeem   -> runRedeem (Confirmed orderOut redeem) (poolOut, pool)
    SwapAction swap       -> runSwap (Confirmed orderOut swap) (poolOut, pool)
