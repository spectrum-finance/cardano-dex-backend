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

import Plutus.V1.Ledger.Crypto
import PlutusTx.Builtins.Internal

data AppConfig = AppConfig
  { kafkaConfig         :: KafkaConsumerConfig
  , topicId             :: TopicId
  , poolsResolverConfig :: PoolsResolverConfig
  , paymentConfig       :: PaymentConfig
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

mkPubKeyHash :: Text -> PubKeyHash
mkPubKeyHash r = PubKeyHash $ BuiltinByteString $ Data.encodeUtf8 r

instance FromDhall PaymentConfig