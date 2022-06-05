module Spectrum.Executor.Config
  ( AppConfig(..)
  , EventSourceConfig(..)
  , loadAppConfig
  ) where

import System.Logging.Hlog
  ( LoggingConfig )

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
import Spectrum.Executor.Types
  ( ConcretePoint )
import Spectrum.Executor.EventSource.Persistence.Config
    ( LedgerStoreConfig )

data EventSourceConfig = EventSourceConfig
  { startAt :: !ConcretePoint
  } deriving (Generic, FromDhall)

data AppConfig = AppConfig
  { ledgerSyncConfig  :: !LedgerSyncConfig
  , eventSourceConfig :: !EventSourceConfig
  , ledgerStoreConfig :: !LedgerStoreConfig
  , loggingConfig     :: !LoggingConfig
  , nodeConfigPath    :: !FilePath
  } deriving (Generic, FromDhall)

loadAppConfig :: MonadIO f => Maybe String -> f AppConfig
loadAppConfig maybePath = liftIO $ input auto path
  where path = T.pack $ fromMaybe "./amm-executor/resources/config.dhall" maybePath
