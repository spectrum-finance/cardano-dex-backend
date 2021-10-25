module Core.Streaming
  ( ConfirmedOrderEvent(..)
  , ConfirmedPoolEvent(..)
  ) where

import Data.Aeson
import GHC.Generics

import ErgoDex.Amm.Orders
import ErgoDex.Amm.Pool
import Cardano.Models 
import Prelude 

import Kafka.Producer
import Kafka.Consumer

import Streaming.Class
import Explorer.Types

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.UTF8 as BSU

data ConfirmedOrderEvent = ConfirmedOrderEvent
  { unAnyOrder :: AnyOrder
  , unTxOut    :: FullTxOut
  , unGix      :: Gix
  } deriving (Generic, FromJSON, ToJSON)

instance ToKafka String ConfirmedOrderEvent where
  toKafka topic k v =
      ProducerRecord topic UnassignedPartition encodedKey encodedValue
    where
      encodedValue = (Just . BS.toStrict . encode) v
      encodedKey   = (Just . BSU.fromString) k

data ConfirmedPoolEvent = ConfirmedPoolEvent
  { unPool  :: Pool
  , unTxOut :: FullTxOut
  , unGix   :: Gix
  } deriving (Generic, FromJSON, ToJSON)