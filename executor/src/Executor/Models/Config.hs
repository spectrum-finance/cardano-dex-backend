module Executor.Models.Config
  ( PoolsResolverConfig(..)
  , PaymentConfig(..)
  , mkPubKeyHash
  , AppConfig(..)
  ) where

import RIO
import Dhall
import Data.Text.Encoding as Data

import Streaming.Config
import Streaming.Types

import WalletAPI.TrustStore
import Plutus.V1.Ledger.Crypto
import Explorer.Config
import NetworkAPI.Config.NodeConfig
import SubmitAPI.Config
import PlutusTx.Builtins.Internal

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
  } deriving (Generic)

instance FromDhall AppConfig

data PoolsResolverConfig = PoolsResolverConfig
  { getHost :: String
  , getPort :: Natural
  } deriving (Generic, Show)

instance FromDhall PoolsResolverConfig

data PaymentConfig = PaymentConfig
  { pubKeyHash :: Text
  , feeAddr    :: Text
  } deriving (Generic, Show)

instance FromDhall PaymentConfig

mkPubKeyHash :: Text -> PubKeyHash
mkPubKeyHash r = PubKeyHash $ BuiltinByteString $ Data.encodeUtf8 r