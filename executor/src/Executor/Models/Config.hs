module Executor.Models.Config
  ( PoolsResolverConfig(..)
  , PaymentConfig(..)
  , mkPubKeyHash
  , AppConfig(..)
  , NodeSocketConfig(..)
  ) where

import RIO
import Dhall
import System.Logging.Hlog
import Data.Text.Encoding as Data

import Streaming.Config
import Streaming.Types

import Plutus.V1.Ledger.Crypto
import PlutusTx.Builtins.Internal

import WalletAPI.TrustStore
import Explorer.Config
import NetworkAPI.Config.NodeConfig
import SubmitAPI.Config

data AppConfig = AppConfig
  { kafkaConfig         :: KafkaConsumerConfig
  , topicId             :: TopicId
  , poolsResolverConfig :: PoolsResolverConfig
  , paymentConfig       :: PaymentConfig
  , explorerConfig      :: ExplorerConfig
  , secretFile          :: SecretFile
  , keyPass             :: KeyPass
  , nodeConfig          :: NodeConfig
  , txAssemblyConfig    :: TxAssemblyConfig
  , nodeSocketConfig    :: NodeSocketConfig
  , loggingConfig       :: LoggingConfig
  } deriving (Generic)

instance FromDhall AppConfig

data PoolsResolverConfig = PoolsResolverConfig
  { getHost :: String
  , getPort :: Natural
  } deriving (Generic, Show)

instance FromDhall PoolsResolverConfig

data NodeSocketConfig = NodeSocketConfig
  { nodeSocketPath :: FilePath
  } deriving (Generic, Show)

instance FromDhall NodeSocketConfig

data PaymentConfig = PaymentConfig
  { pubKeyHash :: Text
  , feeAddr    :: Text
  } deriving (Generic, Show)

mkPubKeyHash :: Text -> PubKeyHash
mkPubKeyHash r = PubKeyHash $ BuiltinByteString $ Data.encodeUtf8 r

instance FromDhall PaymentConfig
