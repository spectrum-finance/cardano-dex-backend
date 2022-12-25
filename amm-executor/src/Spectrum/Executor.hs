{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Spectrum.Executor
  ( runApp
  ) where

import RIO
  ( ReaderT (..), MonadReader (ask), MonadIO (liftIO), void, Alternative (..), MonadPlus (..), newQSem )
import RIO.List
  ( headMaybe )

import System.Posix.Signals
  ( Handler (..)
  , installHandler
  , keyboardSignal
  , raiseSignal
  , softwareTermination
  )

import qualified Control.Concurrent.STM.TBQueue as STM
import qualified Control.Concurrent.STM.TMVar   as STM
import qualified Control.Concurrent.STM.TQueue  as STM
import qualified Control.Concurrent.STM.TVar    as STM
import qualified Control.Monad.STM              as STM
import qualified Control.Concurrent.Async       as Async

import GHC.Generics
  ( Generic )

import Data.Aeson
  ( encode )
import Data.ByteString.Lazy.UTF8
  ( toString )

import Control.Monad.Class.MonadSTM
  ( MonadSTM (..) )
import Control.Monad.Class.MonadST
  ( MonadST )
import Control.Monad.Class.MonadAsync
  ( MonadAsync (..) )
import Control.Monad.Class.MonadFork
  ( MonadThread, MonadFork )
import Control.Monad.Trans.Control
  ( MonadBaseControl )
import Control.Monad.Base
  ( MonadBase )
import Control.Monad.Class.MonadThrow
  ( MonadThrow, MonadMask, MonadCatch )
import Control.Monad.Trans.Resource
  ( ResourceT, runResourceT, MonadUnliftIO )
import Control.Monad.Trans.Class
  ( MonadTrans(lift) )
import qualified Control.Monad.Catch as MC


import Control.Tracer
  ( stdoutTracer, Contravariant (contramap) )
import System.Logging.Hlog
  ( makeLogging, MakeLogging (forComponent), translateMakeLogging )

import Streamly.Prelude as S
  ( drain, parallel )

import Ledger.Tx.CardanoAPI
  ( fromCardanoPaymentKeyHash )
import Ledger
  ( PaymentPubKeyHash(PaymentPubKeyHash), Script, unValidatorScript )
import qualified Cardano.Api as C

import Crypto.Random.Entropy
  ( getEntropy )
import Crypto.Random.Types
  ( MonadRandom(..) )

import NetworkAPI.Service
  ( mkCardanoNetwork )
import SubmitAPI.Config
  ( TxAssemblyConfig )
import NetworkAPI.Types
  ( SocketPath(SocketPath) )
import ErgoDex.Amm.PoolActions
  ( fetchValidatorsV1, mkPoolActions )
import WalletAPI.TrustStore
  ( mkTrustStore )
import WalletAPI.Vault
  ( Vault(getPaymentKeyHash), mkVault )
import WalletAPI.Utxos
  ( mkWalletOutputs' )
import Explorer.Service
  ( mkExplorer )
import Explorer.Config
  ( ExplorerConfig )
import SubmitAPI.Service
  ( mkTransactions )

import Spectrum.LedgerSync.Config
  ( NetworkParameters, LedgerSyncConfig, parseNetworkParameters )
import Spectrum.LedgerSync
  ( mkLedgerSync )
import Cardano.Network.Protocol.NodeToClient.Trace
  ( encodeTraceClient )
import Spectrum.Executor.EventSource.Stream
  ( mkEventSource, EventSource (upstream) )
import Spectrum.Executor.Config
  ( AppConfig(..)
  , loadAppConfig
  , EventSourceConfig
  , TxSubmitConfig(..)
  , Secrets(..)
  , NetworkConfig(..)
  , TxRefs(..)
  )
import Spectrum.Executor.EventSource.Data.TxContext
  ( TxCtx(LedgerCtx) )
import Spectrum.Executor.EventSource.Persistence.Config
  ( LedgerStoreConfig )
import Spectrum.Executor.EventSink.Pipe
  ( mkEventSink, pipe, EventSink )
import Spectrum.Executor.EventSink.Types
  ( voidEventHandler)
import Spectrum.Executor.EventSink.Handlers.Pools
  ( mkNewPoolsHandler )
import Spectrum.Executor.EventSink.Handlers.Orders
  ( mkPendingOrdersHandler, mkEliminatedOrdersHandler )
import Spectrum.Executor.Topic
  ( OneToOneTopic(OneToOneTopic), mkOneToOneTopic, mkNoopTopic )
import Spectrum.Executor.PoolTracker.Persistence.Pools
  ( mkPools )
import Spectrum.Executor.PoolTracker.Process as Tracker
  ( mkPoolTracker, run )
import Spectrum.Executor.PoolTracker.Persistence.Config
  ( PoolStoreConfig )
import Spectrum.Executor.Backlog.Service
  ( mkBacklogService )
import Spectrum.Executor.Backlog.Persistence.Config
  ( BacklogStoreConfig )
import Spectrum.Executor.PoolTracker.Service
  ( mkPoolResolver )
import Spectrum.Executor.OrdersExecutor.Process as Executor
  ( mkOrdersExecutor, run )
import Spectrum.Executor.Backlog.Config
  ( BacklogServiceConfig )
import Spectrum.Executor.Backlog.Process as Backlog
  ( mkBacklog, run, Backlog (run) )
import Spectrum.Executor.Backlog.Persistence.BacklogStore 
  ( mkBacklogStore )
import Streamly.Internal.Data.Stream.Serial 
  ( SerialT(SerialT) )
import Spectrum.Executor.Data.PoolState 
  ( NewPool )
import Spectrum.Executor.Data.State 
  ( Confirmed )
import Spectrum.Executor.Data.OrderState 
  ( OrderInState, OrderState (Pending, Eliminated) )
import Data.Map (Map)
import ErgoDex.PValidators
  ( depositValidator, redeemValidator, swapValidator, poolValidator )
import Plutus.Script.Utils.V2.Scripts (scriptHash)
import qualified Data.Map as Map

data Env f m = Env
  { ledgerSyncConfig   :: !LedgerSyncConfig
  , eventSourceConfig  :: !EventSourceConfig
  , lederHistoryConfig :: !LedgerStoreConfig
  , pstoreConfig       :: !PoolStoreConfig
  , backlogConfig      :: !BacklogServiceConfig
  , txsInsRefs         :: !TxRefs
  , backlogStoreConfig :: !BacklogStoreConfig
  , networkParams      :: !NetworkParameters
  , explorerConfig     :: !ExplorerConfig
  , txSubmitConfig     :: !TxSubmitConfig
  , txAssemblyConfig   :: !TxAssemblyConfig
  , secrets            :: !Secrets
  , mkLogging          :: !(MakeLogging f m)
  , mkLogging'         :: !(MakeLogging m m)
  , networkId          :: !C.NetworkId
  } deriving stock (Generic)

newtype App a = App
  { unApp :: ReaderT (Env Wire App) IO a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (Env Wire App)
    , MonadIO
    , MonadST
    , MonadThread, MonadFork
    , MonadThrow, MC.MonadThrow, MonadCatch, MC.MonadCatch, MonadMask, MC.MonadMask
    , MonadBase IO, MonadBaseControl IO, MonadUnliftIO
    )

type Wire = ResourceT App

runApp :: [String] -> IO ()
runApp args = do
  AppConfig{..} <- loadAppConfig $ headMaybe args
  nparams       <- parseNetworkParameters nodeConfigPath
  mkLogging     <- makeLogging loggingConfig
  let
    networkId =
      if mainnetMode
        then C.Mainnet
        else C.Testnet (C.NetworkMagic (fromIntegral $ cardanoNetworkId networkConfig))
    env =
      Env
        ledgerSyncConfig
        eventSourceConfig
        ledgerStoreConfig
        pstoreConfig
        backlogConfig
        txsInsRefs
        backlogStoreConfig
        nparams
        explorerConfig
        txSubmitConfig
        txAssemblyConfig
        secrets
        (translateMakeLogging (lift . App . lift) mkLogging)
        (translateMakeLogging (App . lift) mkLogging)
        networkId
  runContext env (runResourceT wireApp)

wireApp :: Wire ()
wireApp = interceptSigTerm >> do
  env@Env{..} <- ask
  let tr = contramap (toString . encode . encodeTraceClient) stdoutTracer
  syncSem <- liftIO $ newQSem 1
  lsync   <- lift $ mkLedgerSync (runContext env) tr
  lsource <- mkEventSource lsync
  poolsHandlerTopic      <- forComponent mkLogging "poolsHandlerTopic"
  newOrdersHandlerTopic  <- forComponent mkLogging "newOrdersHandlerTopic"
  elimOrdersHandlerTopic <- forComponent mkLogging "elimOrdersHandlerTopic"
  OneToOneTopic newPoolsRd newPoolsWr     <- mkOneToOneTopic poolsHandlerTopic
  OneToOneTopic newOrdersRd newOrdersWr   <- mkOneToOneTopic newOrdersHandlerTopic
  OneToOneTopic elimOrdersRd elimOrdersWr <- mkOneToOneTopic elimOrdersHandlerTopic
  explorer <- mkExplorer mkLogging explorerConfig
  let
    trustStore = mkTrustStore @_ @C.PaymentKey C.AsPaymentKey (secretFile secrets)
    vault      = mkVault trustStore $ keyPass secrets
  walletOutputs <- mkWalletOutputs' lift mkLogging explorer vault
  executorPkh   <- lift $ fmap fromCardanoPaymentKeyHash (getPaymentKeyHash vault)
  let sockPath = SocketPath $ nodeSocketPath txSubmitConfig
  networkService <- mkCardanoNetwork mkLogging C.BabbageEra epochSlots networkId sockPath
  validators     <- fetchValidatorsV1
  backlogStore   <- mkBacklogStore
  backlogService <- mkBacklogService backlogStore
  pools          <- mkPools
  resolver       <- mkPoolResolver pools
  refScriptsMap  <- mkRefScriptsMap txsInsRefs
  let
    (uPoolsRd, _)   = mkNoopTopic
    (disPoolsRd, _) = mkNoopTopic
    tracker         = mkPoolTracker pools newPoolsRd uPoolsRd disPoolsRd
    backlog         = mkBacklog backlogService newOrdersRd elimOrdersRd
    transactions    = mkTransactions networkService networkId refScriptsMap walletOutputs vault txAssemblyConfig
    poolActions     = mkPoolActions (PaymentPubKeyHash executorPkh) validators
  executor <- mkOrdersExecutor backlogService syncSem transactions explorer resolver poolActions
  pendingOrdersLogging <- forComponent mkLogging "PendingOrdersHandler"
  poolHandlerLogging   <- forComponent mkLogging "PoolHandler"
  let
    poolsHan      = mkNewPoolsHandler newPoolsWr poolHandlerLogging
    newOrdersHan  = mkPendingOrdersHandler newOrdersWr syncSem pendingOrdersLogging backlogConfig networkParams
    execOrdersHan = mkEliminatedOrdersHandler backlogStore backlogConfig networkParams elimOrdersWr
    lsink         = mkEventSink [poolsHan, newOrdersHan, execOrdersHan] voidEventHandler
  lift . S.drain $
    S.parallel (Tracker.run tracker) $
    S.parallel (pipe lsink . upstream $ lsource) $
    S.parallel (Backlog.run backlog) $
    Executor.run executor

epochSlots :: C.ConsensusModeParams C.CardanoMode
epochSlots = C.CardanoModeParams $ C.EpochSlots 21600

mkRefScriptsMap :: MonadIO f => TxRefs -> f (Map Script C.TxIn)
mkRefScriptsMap TxRefs{..} = do
  swapV    <- unValidatorScript <$> swapValidator
  depositV <- unValidatorScript <$> depositValidator
  redeemV  <- unValidatorScript <$> redeemValidator
  poolV    <- unValidatorScript <$> poolValidator
  pure $ Map.fromList
    [ (swapV, swapRef)
    , (depositV, depositRef)
    , (redeemV, redeemRef)
    , (poolV, poolRef)
    ]

runContext :: Env Wire App -> App a -> IO a
runContext env app = runReaderT (unApp app) env

interceptSigTerm :: Wire ()
interceptSigTerm =
    lift $ liftIO $ void $ installHandler softwareTermination handler Nothing
  where
    handler = CatchOnce $ raiseSignal keyboardSignal

instance MonadRandom App where
    getRandomBytes = liftIO . getEntropy

newtype WrappedSTM a = WrappedSTM { unwrapSTM :: STM.STM a }
    deriving newtype (Functor, Applicative, Alternative, Monad, MonadPlus, MonadThrow)

instance MonadSTM App where
  type STM     App = WrappedSTM
  type TVar    App = STM.TVar
  type TMVar   App = STM.TMVar
  type TQueue  App = STM.TQueue
  type TBQueue App = STM.TBQueue

  atomically      = App . lift . STM.atomically . unwrapSTM
  retry           = WrappedSTM STM.retry
  orElse          = \a0 a1 -> WrappedSTM (STM.orElse (unwrapSTM a0) (unwrapSTM a1))
  check           = WrappedSTM . STM.check

  newTVar         = WrappedSTM . STM.newTVar
  newTVarIO       = App . lift . STM.newTVarIO
  readTVar        = WrappedSTM . STM.readTVar
  readTVarIO      = App . lift . STM.readTVarIO
  writeTVar       = \a0 -> WrappedSTM . STM.writeTVar a0
  modifyTVar      = \a0 -> WrappedSTM . STM.modifyTVar a0
  modifyTVar'     = \a0 -> WrappedSTM . STM.modifyTVar' a0
  stateTVar       = \a0 -> WrappedSTM . STM.stateTVar a0
  swapTVar        = \a0 -> WrappedSTM . STM.swapTVar a0

  newTMVar        = WrappedSTM . STM.newTMVar
  newTMVarIO      = App . lift . STM.newTMVarIO
  newEmptyTMVar   = WrappedSTM STM.newEmptyTMVar
  newEmptyTMVarIO = App (lift STM.newEmptyTMVarIO)
  takeTMVar       = WrappedSTM . STM.takeTMVar
  tryTakeTMVar    = WrappedSTM . STM.tryTakeTMVar
  putTMVar        = \a0 -> WrappedSTM . STM.putTMVar a0
  tryPutTMVar     = \a0 -> WrappedSTM . STM.tryPutTMVar a0
  readTMVar       = WrappedSTM . STM.readTMVar
  tryReadTMVar    = WrappedSTM . STM.tryReadTMVar
  swapTMVar       = \a0 -> WrappedSTM . STM.swapTMVar a0
  isEmptyTMVar    = WrappedSTM . STM.isEmptyTMVar

  newTQueue       = WrappedSTM STM.newTQueue
  newTQueueIO     = App (lift STM.newTQueueIO)
  readTQueue      = WrappedSTM . STM.readTQueue
  tryReadTQueue   = WrappedSTM . STM.tryReadTQueue
  peekTQueue      = WrappedSTM . STM.peekTQueue
  tryPeekTQueue   = WrappedSTM . STM.tryPeekTQueue
  flushTBQueue    = WrappedSTM . STM.flushTBQueue
  writeTQueue     = \a0 -> WrappedSTM . STM.writeTQueue a0
  isEmptyTQueue   = WrappedSTM . STM.isEmptyTQueue

  newTBQueue      = WrappedSTM . STM.newTBQueue
  newTBQueueIO    = App . lift . STM.newTBQueueIO
  readTBQueue     = WrappedSTM . STM.readTBQueue
  tryReadTBQueue  = WrappedSTM . STM.tryReadTBQueue
  peekTBQueue     = WrappedSTM . STM.peekTBQueue
  tryPeekTBQueue  = WrappedSTM . STM.tryPeekTBQueue
  writeTBQueue    = \a0 -> WrappedSTM . STM.writeTBQueue a0
  lengthTBQueue   = WrappedSTM . STM.lengthTBQueue
  isEmptyTBQueue  = WrappedSTM . STM.isEmptyTBQueue
  isFullTBQueue   = WrappedSTM . STM.isFullTBQueue

newtype WrappedAsync a = WrappedAsync { unwrapAsync :: Async.Async a }
    deriving newtype (Functor)

instance MonadAsync App where
  type Async App  = WrappedAsync
  async           = \(App (ReaderT m)) -> App (ReaderT $ \r -> WrappedAsync <$> async (m r))
  asyncThreadId   = Async.asyncThreadId . unwrapAsync
  pollSTM         = WrappedSTM . Async.pollSTM . unwrapAsync
  waitCatchSTM    = WrappedSTM . Async.waitCatchSTM . unwrapAsync
  cancel          = App . lift . Async.cancel . unwrapAsync
  cancelWith      = \a0 -> App . lift . Async.cancelWith (unwrapAsync a0)
  asyncWithUnmask = \restore -> App $ ReaderT $ \r ->
      fmap WrappedAsync $ Async.asyncWithUnmask $ \unmask ->
        runReaderT (unApp (restore (liftF unmask))) r
    where
      liftF :: (IO a -> IO a) -> App a -> App a
      liftF g (App (ReaderT f)) = App (ReaderT (g . f))