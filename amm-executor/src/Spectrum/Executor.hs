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
import Control.Monad.Class.MonadThrow ( MonadThrow, MonadMask, MonadCatch )
import Control.Monad.Trans.Resource   ( MonadResource )
import qualified Control.Monad.Catch as MC

import Control.Tracer
  ( stdoutTracer, Contravariant (contramap) )
import System.Logging.Hlog
  ( makeLogging, MakeLogging, translateMakeLogging )

import Streamly.Prelude as S (drain)

import Spectrum.LedgerSync.Config ( NetworkParameters, LedgerSyncConfig, parseNetworkParameters )
import Spectrum.LedgerSync (mkLedgerSync)
import Cardano.Network.Protocol.NodeToClient.Trace (encodeTraceClient)
import Data.Aeson (encode)
import Data.ByteString.Lazy.UTF8 (toString)
import Spectrum.Executor.DataSource.Stream (mkDataSource, DataSource (upstream))
import Spectrum.Executor.Config (AppConfig(..), loadAppConfig, DataSourceConfig)
import RIO.List (headMaybe)
import Control.Monad.Trans.Class (MonadTrans(lift))

data Env m = Env
  { ledgerSyncConfig :: !LedgerSyncConfig
  , dataSourceConfig :: !DataSourceConfig
  , networkParams    :: !NetworkParameters
  , mkLogging        :: !(MakeLogging m m)
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
    , MonadThrow, MC.MonadThrow, MonadCatch, MonadMask
    , MonadBase IO, MonadBaseControl IO
    )

runApp :: [String] -> IO ()
runApp args = do
  AppConfig{..} <- loadAppConfig $ headMaybe args
  nparams       <- parseNetworkParameters nodeConfigPath
  mkLogging     <- makeLogging loggingConfig
  let
    env =
      Env ledgerSyncConfig dataSourceConfig nparams
        $ translateMakeLogging (App . lift) mkLogging
  runContext env wireApp

wireApp :: App ()
wireApp = interceptSigTerm >> do
  env <- ask
  let tr = contramap (toString . encode . encodeTraceClient) stdoutTracer
  lsync <- mkLedgerSync (runContext env) tr
  ds    <- mkDataSource lsync
  S.drain $ upstream ds

runContext :: Env App -> App a -> IO a
runContext env app = runReaderT (unApp app) env

interceptSigTerm :: App ()
interceptSigTerm =
    liftIO $ void $ installHandler softwareTermination handler Nothing
  where
    handler = CatchOnce $ raiseSignal keyboardSignal
