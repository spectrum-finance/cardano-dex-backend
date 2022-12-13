module Spectrum.Executor.Config
  ( AppConfig(..)
  , EventSourceConfig(..)
  , TxSubmitConfig(..)
  , NetworkConfig(..)
  , Secrets(..)
  , TxRefs(..)
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
import Dhall
  ( FromDhall )
import Dhall.Core
  ( Expr(..), Chunks(..) )
import qualified Dhall     as D
import qualified Data.Text as T
import Text.Parsec

import Cardano.Api.Shelley 
  ( TxIn )

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
import Spectrum.Common.Parsers (parseTxIn)

data NetworkConfig = NetworkConfig
  { cardanoNetworkId :: !Natural 
  } deriving (Generic, FromDhall)

data EventSourceConfig = EventSourceConfig
  { startAt :: !ConcretePoint
  } deriving (Generic, FromDhall)

data TxSubmitConfig = TxSubmitConfig
  { nodeSocketPath :: !FilePath
  } deriving (Generic, FromDhall)

data TxRefs = TxRefs
  { swapRef    :: !TxIn
  , depositRef :: !TxIn 
  , redeemRef  :: !TxIn
  , poolRef    :: !TxIn
  } deriving (Generic, FromDhall)

instance FromDhall TxIn where
  autoWith _ = D.Decoder{..}
    where
      extract (TextLit (Chunks [] t)) = either (D.extractError . T.pack . show) pure $ parse parseTxIn "" (T.unpack t)
      extract expr                    = D.typeError expected expr

      expected = pure Text

data Secrets = Secrets
  { secretFile :: !SecretFile
  , keyPass    :: !KeyPass
  } deriving (Generic, FromDhall)

data AppConfig = AppConfig
  { ledgerSyncConfig   :: !LedgerSyncConfig
  , eventSourceConfig  :: !EventSourceConfig
  , ledgerStoreConfig  :: !LedgerStoreConfig
  , nodeConfigPath     :: !FilePath
  , txsInsRefs         :: !TxRefs
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
