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
  ( (&), MonadReader, MonadUnliftIO, MonadIO (liftIO), QSem, (<&>) )
import qualified RIO.List as List
import Streamly.Prelude as S
  ( repeatM, mapM, MonadAsync, IsStream, before, drain, fromEffect )
import Control.Monad.Catch
  ( MonadThrow, SomeException, MonadCatch, catches, Handler (Handler) )
import RIO
  ( atomicModifyIORef' )

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
  ( PoolActions (PoolActions, runDeposit, runRedeem, runSwap)
  , OrderExecErr (EmptyPool, PoolNotFoundInFinalTx, PriceTooHigh, InsufficientPoolLqForSwap)
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
import qualified System.Logging.Hlog as Trace
import Spectrum.Executor.OrdersExecutor.Service (OrdersExecutorService(..))

newtype OrdersExecutor s m = OrdersExecutor
  { run :: s m ()
  }

mkOrdersExecutor
  :: forall f m s env.
    ( IsStream s
    , MonadAsync m
    , MonadReader env f
    , HasType (MakeLogging f m) env)
  => BacklogService m
  -> OrdersExecutorService m
  -> f (OrdersExecutor s m)
mkOrdersExecutor backlog executorService = do
  MakeLogging{..} <- askContext
  logging         <- forComponent "Bots.OrdersExecutor"
  pure $ OrdersExecutor
    { run = run' logging backlog executorService
    }

run'
  :: forall s m. (IsStream s, MonadAsync m)
  => Logging m
  -> BacklogService m
  -> OrdersExecutorService m
  -> s m ()
run' Logging{..} BacklogService{..} OrdersExecutorService{..} =
  S.repeatM (liftIO (threadDelay 5000000) >> atomicModifyIORef' getQueue (\queue ->
        case queue of
          [] -> ([], Nothing)
          [xs] -> ([], Just xs)
          (x:xs) -> (xs, Just x)
      )) & S.mapM (\case
      Just order ->
        pure $ Just order
      Nothing    ->
        tryAcquire
    ) & S.mapM (\case
      Just orderWithCreationTime@(OrderWithCreationTime order _) ->
        infoM ("Going to execute order for pool" ++ show (orderId order)) >>
          execute orderWithCreationTime
      Nothing    ->
        pure ()
    )
