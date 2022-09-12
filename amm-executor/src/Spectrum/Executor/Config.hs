module Spectrum.Executor.Config
  ( AppConfig(..)
  , EventSourceConfig(..)
  , TxSubmitConfig(..)
  , NetworkConfig(..)
  , Secrets(..)
  , loadAppConfig
  ) where

import System.Logging.Hlog
  ( LoggingConfig )

import RIO 
  ( Natural )
import GHC.Generics
  ( Generic )
import Dhall
  ( FromDhall, input, auto )

import Control.Monad.IO.Class
  ( MonadIO (liftIO) )
import Data.Maybe
  ( fromMaybe )
import qualified Data.Text as T

import Spectrum.LedgerSync.Config
  ( LedgerSyncConfig )
import Spectrum.Executor.EventSource.Types
  ( ConcretePoint )
import Spectrum.Executor.EventSource.Persistence.Config
    ( LedgerStoreConfig )
import Spectrum.Executor.PoolTracker.Persistence.Config
  ( PoolStoreConfig )
import Spectrum.Executor.Backlog.Config
  ( BacklogServiceConfig )
import Spectrum.Executor.Backlog.Persistence.Config
  ( BacklogStoreConfig )
import SubmitAPI.Config
  ( TxAssemblyConfig )
import Explorer.Config
  ( ExplorerConfig )
import WalletAPI.TrustStore (SecretFile, KeyPass)

data NetworkConfig = NetworkConfig
  { cardanoNetworkId :: !Natural 
  } deriving (Generic, FromDhall)

data EventSourceConfig = EventSourceConfig
  { startAt :: !ConcretePoint
  } deriving (Generic, FromDhall)

data TxSubmitConfig = TxSubmitConfig
  { nodeSocketPath :: !FilePath
  } deriving (Generic, FromDhall)

data Secrets = Secrets
  { secretFile :: !SecretFile
  , keyPass    :: !KeyPass
  } deriving (Generic, FromDhall)

data AppConfig = AppConfig
  { ledgerSyncConfig   :: !LedgerSyncConfig
  , eventSourceConfig  :: !EventSourceConfig
  , ledgerStoreConfig  :: !LedgerStoreConfig
  , nodeConfigPath     :: !FilePath
  , networkConfig      :: !NetworkConfig
  , loggingConfig      :: !LoggingConfig
  , pstoreConfig       :: !PoolStoreConfig
  , backlogConfig      :: !BacklogServiceConfig
  , backlogStoreConfig :: !BacklogStoreConfig
  , explorerConfig     :: !ExplorerConfig
  , txSubmitConfig     :: !TxSubmitConfig
  , txAssemblyConfig   :: !TxAssemblyConfig
  , secrets            :: !Secrets
  , mainnetMode        :: !Bool
  } deriving (Generic, FromDhall)

loadAppConfig :: MonadIO f => Maybe String -> f AppConfig
loadAppConfig maybePath = liftIO $ input auto path
  where path = T.pack $ fromMaybe "./amm-executor/resources/config.dhall" maybePath
