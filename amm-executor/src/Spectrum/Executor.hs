{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Spectrum.Executor
  ( runApp
  ) where

import RIO ( ReaderT (runReaderT), MonadReader (ask), MonadIO (liftIO), void )

import System.Posix.Signals
  ( Handler (..)
  , installHandler
  , keyboardSignal
  , raiseSignal
  , softwareTermination
  )

import GHC.Generics (Generic)

import Control.Monad.Class.MonadSTM   ( MonadSTM )
import Control.Monad.Class.MonadST    ( MonadST )
import Control.Monad.Class.MonadAsync ( MonadAsync )
import Control.Monad.Class.MonadFork  ( MonadThread, MonadFork )
import Control.Monad.Trans.Control    ( MonadBaseControl )
import Control.Monad.Base             ( MonadBase )
import Control.Monad.Class.MonadThrow ( MonadThrow )
import Control.Monad.Trans.Resource   ( MonadResource )
import qualified Control.Monad.Catch as MC

import Control.Tracer (stdoutTracer, Contravariant (contramap))
import System.Logging.Hlog (LoggingConfig, makeLogging)

import Streamly.Prelude as S (drain)

import Spectrum.LedgerSync.Config ( NetworkParameters, LedgerSyncConfig, parseNetworkParameters )
import Spectrum.LedgerSync (mkLedgerSync)
import Cardano.Network.Protocol.NodeToClient.Trace (encodeTraceClient)
import Data.Aeson (encode)
import Data.ByteString.Lazy.UTF8 (toString)
import Spectrum.Executor.DataSource (mkDataSource, DataSource (upstream))
import Spectrum.Executor.Config (AppConfig(..), loadAppConfig)
import RIO.List (headMaybe)

data Env m = Env
  { ledgerSyncConfig :: !LedgerSyncConfig
  , loggingConfig    :: !LoggingConfig
  , networkParams    :: !NetworkParameters
  } deriving stock (Generic)

newtype App a = App
  { unApp :: ReaderT (Env App) IO a
  } deriving anyclass MonadResource
    deriving newtype
    ( Functor, Applicative, Monad
    , MonadReader (Env App)
    , MonadIO
    , MonadSTM, MonadST
    , MonadAsync, MonadThread, MonadFork
    , MonadThrow, MC.MonadThrow
    , MonadBase IO, MonadBaseControl IO
    )

runApp :: [String] -> IO ()
runApp args = do
  AppConfig{..} <- loadAppConfig $ headMaybe args
  nparams       <- parseNetworkParameters nodeConfigPath
  let env = Env ledgerSyncConfig loggingConfig nparams
  runContext env wireApp

wireApp :: App ()
wireApp = interceptSigTerm >> do
  env@Env{loggingConfig} <- ask
  mkLogging <- makeLogging loggingConfig
  let tr = contramap (toString . encode . encodeTraceClient) stdoutTracer
  lsync <- mkLedgerSync (runContext env) tr
  ds    <- mkDataSource mkLogging lsync
  S.drain (upstream ds)

runContext :: Env App -> App a -> IO a
runContext env app = runReaderT (unApp app) env

interceptSigTerm :: App ()
interceptSigTerm =
    liftIO $ void $ installHandler softwareTermination handler Nothing
  where
    handler = CatchOnce $ raiseSignal keyboardSignal
