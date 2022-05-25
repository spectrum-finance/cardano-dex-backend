module Spectrum.Executor.Config
  ( AppConfig(..)
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

data AppConfig = AppConfig
  { ledgerSyncConfig :: !LedgerSyncConfig
  , loggingConfig    :: !LoggingConfig
  , nodeConfigPath   :: !FilePath
  } deriving (Generic, FromDhall)

loadAppConfig :: MonadIO f => Maybe String -> f AppConfig
loadAppConfig maybePath = liftIO $ input auto path
  where path = T.pack $ fromMaybe "./amm-executor/resources/config.dhall" maybePath
