{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Spectrum.Executor
  ( runApp
  ) where

import RIO
  ( ReaderT (runReaderT), MonadReader (ask), MonadIO (liftIO), void )
import RIO.List
  ( headMaybe )

import System.Posix.Signals
  ( Handler (..)
  , installHandler
  , keyboardSignal
  , raiseSignal
  , softwareTermination
  )

import GHC.Generics
  ( Generic )

import Data.Aeson
  ( encode )
import Data.ByteString.Lazy.UTF8
  ( toString )

import Control.Monad.Class.MonadSTM
  ( MonadSTM )
import Control.Monad.Class.MonadST
  ( MonadST )
import Control.Monad.Class.MonadAsync
  ( MonadAsync )
import Control.Monad.Class.MonadFork
  ( MonadThread, MonadFork )
import Control.Monad.Trans.Control
  ( MonadBaseControl )
import Control.Monad.Base
  ( MonadBase )
import Control.Monad.Class.MonadThrow
  ( MonadThrow, MonadMask, MonadCatch )
import Control.Monad.Trans.Resource
  ( MonadResource, ResourceT, runResourceT, MonadUnliftIO )
import Control.Monad.Trans.Class
  ( MonadTrans(lift) )
import qualified Control.Monad.Catch as MC


import Control.Tracer
  ( stdoutTracer, Contravariant (contramap) )
import System.Logging.Hlog
  ( makeLogging, MakeLogging, translateMakeLogging )

import Streamly.Prelude as S (drain)

import Spectrum.LedgerSync.Config
  ( NetworkParameters, LedgerSyncConfig, parseNetworkParameters )
import Spectrum.LedgerSync
  ( mkLedgerSync )
import Cardano.Network.Protocol.NodeToClient.Trace
  ( encodeTraceClient )
import Spectrum.Executor.EventSource.Stream
  ( mkEventSource, EventSource (upstream) )
import Spectrum.Executor.Config
  ( AppConfig(..), loadAppConfig, EventSourceConfig )
import Spectrum.Executor.EventSource.Persistence.Config
  ( LedgerStoreConfig )

data Env r m = Env
  { ledgerSyncConfig   :: !LedgerSyncConfig
  , eventSourceConfig  :: !EventSourceConfig
  , lederHistoryConfig :: !LedgerStoreConfig
  , networkParams      :: !NetworkParameters
  , mkLogging          :: !(MakeLogging r m)
  , mkLogging'         :: !(MakeLogging m m)
  } deriving stock (Generic)

type Wire = ResourceT App 

newtype App a = App
  { unApp :: ReaderT (Env Wire App) IO a
  } deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (Env Wire App)
    , MonadIO
    , MonadSTM, MonadST
    , MonadAsync, MonadThread, MonadFork
    , MonadThrow, MC.MonadThrow, MonadCatch, MonadMask
    , MonadBase IO, MonadBaseControl IO, MonadUnliftIO
    )

runApp :: [String] -> IO ()
runApp args = do
  AppConfig{..} <- loadAppConfig $ headMaybe args
  nparams       <- parseNetworkParameters nodeConfigPath
  mkLogging     <- makeLogging loggingConfig
  let
    env =
      Env ledgerSyncConfig eventSourceConfig ledgerStoreConfig nparams
        (translateMakeLogging (lift . App . lift) mkLogging)
        (translateMakeLogging (App . lift) mkLogging)
  runContext env (runResourceT wireApp)

wireApp :: Wire ()
wireApp = lift interceptSigTerm >> do
  env <- lift ask
  let tr = contramap (toString . encode . encodeTraceClient) stdoutTracer
  lsync <- lift $ mkLedgerSync (runContext env) tr
  ds    <- mkEventSource lsync
  lift $ S.drain $ upstream ds

runContext :: Env Wire App -> App a -> IO a
runContext env app = runReaderT (unApp app) env

interceptSigTerm :: App ()
interceptSigTerm =
    liftIO $ void $ installHandler softwareTermination handler Nothing
  where
    handler = CatchOnce $ raiseSignal keyboardSignal
