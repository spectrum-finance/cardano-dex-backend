{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Spectrum.Executor
  ( runApp
  ) where

import RIO
  ( ReaderT (..), MonadReader (ask), MonadIO (liftIO), void, Alternative (..), MonadPlus (..), newQSem, QSem, waitQSem, runReaderT )
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
  ( makeLogging, MakeLogging (forComponent, MakeLogging), translateMakeLogging, Logging (..), Loggable (toLog), LoggingConfig )
import qualified System.Log.Logger as SL (errorM, infoM, debugM, warningM)

import Streamly.Prelude as S
  ( drain, parallel, mapM, SerialT, before, fromEffect )

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
  ( TxAssemblyConfig, UnsafeEvalConfig )
import NetworkAPI.Types
  ( SocketPath(SocketPath) )
import WalletAPI.TrustStore
  ( mkTrustStore )
import WalletAPI.Vault
  ( Vault(getPaymentKeyHash), mkVault )
import WalletAPI.Utxos
  ( mkPersistentWalletOutputs, mkWalletOutputs )
import ErgoDex.Amm.PoolActions
  ( mkPoolActions )
import Explorer.Service
  ( mkExplorer )
import Explorer.Config
  ( ExplorerConfig )
import SubmitAPI.Service
  ( mkTransactions )
import NetworkAPI.HttpService
  ( HttpServiceConfig(..), mkHttpCardanoNetwork )

import Spectrum.LedgerSync.Config
  ( NetworkParameters, NodeSocketConfig(..), parseNetworkParameters )
import Spectrum.LedgerSync
  ( mkLedgerSync, LedgerSync )
import Cardano.Network.Protocol.NodeToClient.Trace
  ( encodeTraceClient )
import Spectrum.EventSource.Stream
  ( mkLedgerEventSource, mkMempoolTxEventSource, EventSource (upstream) )
import Spectrum.Config
  ( EventSourceConfig )
import Spectrum.Executor.Config
  ( AppConfig(..)
  , loadAppConfig
  , Secrets(..)
  , NetworkConfig(..)
  , TxRefs(..)
  , ScriptsConfig (..)
  )
import Spectrum.EventSource.Persistence.Config
  ( LedgerStoreConfig )
import Spectrum.Executor.EventSink.Pipe
  ( mkEventSink, pipe )
import Spectrum.Executor.EventSink.Types
  ( voidEventHandler)
import Spectrum.Executor.EventSink.Handlers.Pools
  ( mkNewPoolsHandler )
import Spectrum.Executor.EventSink.Handlers.Orders
  ( mkPendingOrdersHandler, mkEliminatedOrdersHandler, mkMempoolPendingOrdersHandler )
import Spectrum.Topic
  ( OneToOneTopic(OneToOneTopic), mkOneToOneTopic, mkNoopTopic )
import Spectrum.Executor.PoolTracker.Persistence.Pools
  ( mkPools, mkNonPersistentPools )
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
  ( mkBacklogStore, mkBacklogStoreNonPersist )
import Data.Map (Map)
import qualified Data.Map as Map
import WalletAPI.UtxoStoreConfig
  ( UtxoStoreConfig )
import Spectrum.Executor.Scripts
  ( ScriptsValidators(..), mkScriptsValidators, scriptsValidators2AmmValidatorsV1, scriptsValidators2AmmValidatorsV2 )
import Spectrum.Executor.OrdersExecutor.Service (mkOrdersExecutorService)
import Spectrum.Executor.OrdersExecutor.RefInputs (mkRefInputs)

data Env = Env
  { mainnetMode        :: !Bool
  , nodeSocketConfig   :: !NodeSocketConfig
  , eventSourceConfig  :: !EventSourceConfig
  , lederHistoryConfig :: !LedgerStoreConfig
  , pstoreConfig       :: !PoolStoreConfig
  , backlogConfig      :: !BacklogServiceConfig
  , txsInsRefs         :: !TxRefs
  , scriptsConfig      :: !ScriptsConfig
  , backlogStoreConfig :: !BacklogStoreConfig
  , networkParams      :: !NetworkParameters
  , explorerConfig     :: !ExplorerConfig
  , txAssemblyConfig   :: !TxAssemblyConfig
  , utxoStoreConfig    :: !UtxoStoreConfig
  , secrets            :: !Secrets
  , mkLogging          :: !(MakeLogging F IO)
  , mkLogging'         :: !(MakeLogging IO IO)
  , networkId          :: !C.NetworkId
  , unsafeEval         :: !UnsafeEvalConfig
  , httpSubmit         :: !HttpServiceConfig
  } deriving stock (Generic)

type F = ReaderT Env IO

newtype App a = App
  { unApp :: F a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader Env
    , MonadIO
    , MonadST
    , MonadThread, MonadFork
    , MonadThrow, MC.MonadThrow, MonadCatch, MC.MonadCatch, MonadMask, MC.MonadMask
    , MonadBase IO, MonadBaseControl IO, MonadUnliftIO
    )

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
        mainnetMode
        nodeSocketConfig
        eventSourceConfig
        ledgerStoreConfig
        pstoreConfig
        backlogConfig
        txsInsRefs
        scriptsConfig
        backlogStoreConfig
        nparams
        explorerConfig
        txAssemblyConfig
        utxoStoreConfig
        secrets
        (translateMakeLogging liftIO mkLogging)
        mkLogging
        networkId
        unsafeEval
        httpSubmit
  runContext env wireApp

wireApp :: App ()
wireApp = App { unApp = interceptSigTerm >> do
  env@Env{..} <- ask
  let tr = contramap (toString . encode . encodeTraceClient) stdoutTracer
  syncSem <- liftIO $ newQSem 1
  liftIO $ waitQSem syncSem
  lsync   <- lift $ mkLedgerSync id tr mkLogging' nodeSocketConfig networkParams
  lsource <- mkLedgerEventSource lsync liftIO
  msource <- mkMempoolTxEventSource lsync
  poolsHandlerTopic      <- forComponent mkLogging "Bots.poolsHandlerTopic"
  newOrdersHandlerTopic  <- forComponent mkLogging "Bots.newOrdersHandlerTopic"
  newOrdersMHandlerTopic <- forComponent mkLogging "Bots.newOrdersMHandlerTopic"
  elimOrdersHandlerTopic <- forComponent mkLogging "Bots.elimOrdersHandlerTopic"
  OneToOneTopic newPoolsRd newPoolsWr     <- mkOneToOneTopic poolsHandlerTopic
  OneToOneTopic newOrdersRd newOrdersWr   <- mkOneToOneTopic newOrdersHandlerTopic
  OneToOneTopic newOrderMsRd newOrdersMWr <- mkOneToOneTopic newOrdersMHandlerTopic
  OneToOneTopic elimOrdersRd elimOrdersWr <- mkOneToOneTopic elimOrdersHandlerTopic
  explorer <- mkExplorer mkLogging explorerConfig
  let
    trustStore = mkTrustStore @_ @C.PaymentKey C.AsPaymentKey (secretFile secrets)
    vault      = mkVault trustStore $ keyPass secrets :: Vault IO
  pkh <- lift $ getPaymentKeyHash vault
  walletOutputs <- mkWalletOutputs mkLogging explorer pkh
  executorPkh   <- lift $ fmap fromCardanoPaymentKeyHash (getPaymentKeyHash vault)
  let sockPath = SocketPath $ nodeSocketPath nodeSocketConfig
  networkService <- mkCardanoNetwork mkLogging C.BabbageEra epochSlots networkId sockPath
  backlogStore   <- mkBacklogStoreNonPersist
  backlogService <- mkBacklogService backlogStore
  pools          <- mkNonPersistentPools
  resolver       <- mkPoolResolver pools

  scriptsValidators   <- mkScriptsValidators scriptsConfig
  transactionsLogging <- forComponent mkLogging "Bots.Transactions"
  httpSubmitNetwork <- mkHttpCardanoNetwork mkLogging httpSubmit
  let
    validatorsV1    = scriptsValidators2AmmValidatorsV1 scriptsValidators
    validatorsV2    = scriptsValidators2AmmValidatorsV2 scriptsValidators
    refScriptsMap   = mkRefScriptsMap txsInsRefs scriptsValidators
    (uPoolsRd, _)   = mkNoopTopic
    (disPoolsRd, _) = mkNoopTopic
    tracker         = mkPoolTracker pools newPoolsRd uPoolsRd disPoolsRd
    transactions    = mkTransactions unsafeEval transactionsLogging networkService httpSubmitNetwork networkId refScriptsMap walletOutputs vault txAssemblyConfig
    poolActionsV1   = mkPoolActions unsafeEval (PaymentPubKeyHash executorPkh) validatorsV1
    poolActionsV2   = mkPoolActions unsafeEval (PaymentPubKeyHash executorPkh) validatorsV2
  refInputs <- liftIO $ mkRefInputs txsInsRefs explorer
  executorService <- mkOrdersExecutorService backlogService transactions backlogConfig explorer resolver poolActionsV1 poolActionsV2 refInputs
  executor <- mkOrdersExecutor backlogService executorService
  pendingOrdersLogging <- forComponent mkLogging "Bots.PendingOrdersHandler"
  mempoolOrdersLogging <- forComponent mkLogging "Bots.MempoolOrdersHandler"
  poolHandlerLogging   <- forComponent mkLogging "Bots.PoolHandler"
  let
    backlog       = mkBacklog backlogService newOrderMsRd newOrdersRd elimOrdersRd
    poolsHan      = mkNewPoolsHandler newPoolsWr poolHandlerLogging scriptsValidators
    newOrdersHan  = mkPendingOrdersHandler newOrdersWr syncSem pendingOrdersLogging mainnetMode backlogConfig networkParams
    newMOrdersHan = mkMempoolPendingOrdersHandler newOrdersMWr mempoolOrdersLogging mainnetMode backlogConfig networkParams executorService
    execOrdersHan = mkEliminatedOrdersHandler backlogStore backlogConfig networkParams elimOrdersWr
    lsink         = mkEventSink [poolsHan, newOrdersHan, execOrdersHan] voidEventHandler
    msink         = mkEventSink [newMOrdersHan] voidEventHandler
    app = S.parallel (pipe msink . upstream $ msource) $ Executor.run executor
  lift . S.drain $
    S.parallel (Tracker.run tracker) $
    S.parallel (pipe lsink . upstream $ lsource) $
    S.parallel (Backlog.run backlog) $
    runExecutorWithMempoolProcessing syncSem app
}

runExecutorWithMempoolProcessing :: QSem -> SerialT IO () -> SerialT IO ()
runExecutorWithMempoolProcessing sem = S.before (liftIO $ waitQSem sem)

epochSlots :: C.ConsensusModeParams C.CardanoMode
epochSlots = C.CardanoModeParams $ C.EpochSlots 21600

mkRefScriptsMap :: TxRefs -> ScriptsValidators -> Map Script C.TxIn
mkRefScriptsMap TxRefs{..} ScriptsValidators{..} =
  let
    swapV    = unValidatorScript swapValidator
    depositV = unValidatorScript depositValidator
    redeemV  = unValidatorScript redeemValidator
    poolV1   = unValidatorScript poolV1Validator
    poolV2   = unValidatorScript poolV2Validator
  in Map.fromList
    [ (swapV, swapRef)
    , (depositV, depositRef)
    , (redeemV, redeemRef)
    , (poolV1, poolV1Ref)
    , (poolV2, poolV2Ref)
    ]

runContext :: Env -> App a -> IO a
runContext env app = runReaderT (unApp app) env

interceptSigTerm :: ReaderT (Env) IO ()
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