module Streaming.Events
  ( ConfirmedOrderEvent(..)
  , ConfirmedPoolEvent(..)
  ) where

import Data.Aeson
import GHC.Generics
import Prelude 
import Kafka.Producer
import Kafka.Consumer
import RIO

import ErgoDex.Amm.Orders
import ErgoDex.Amm.Pool
import CardanoTx.Models 
import Streaming.Class
import Explorer.Types

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString      as ByteString

data ConfirmedOrderEvent = ConfirmedOrderEvent
  { anyOrder :: AnyOrder
  , txOut    :: FullTxOut
  , gix      :: Gix
  } deriving (Generic, FromJSON, ToJSON)

instance ToKafka PoolId ConfirmedOrderEvent where
  toKafka topic k v =
      ProducerRecord topic UnassignedPartition encodedKey encodedValue
    where
      encodedValue = asKey v
      encodedKey   = asKey k

instance FromKafka PoolId ConfirmedOrderEvent where
  fromKafka consumerRecord =
      keyMaybe >>= (\key -> valueMaybe <&> (key,))
    where
      keyMaybe   = (fromKey $ crKey consumerRecord) :: Maybe PoolId
      valueMaybe = (fromKey $ crValue consumerRecord) :: Maybe ConfirmedOrderEvent

data ConfirmedPoolEvent = ConfirmedPoolEvent
  { pool  :: Pool
  , txOut :: FullTxOut
  , gix   :: Gix
  } deriving (Generic, FromJSON, ToJSON, Show)

instance ToKafka PoolId ConfirmedPoolEvent where
  toKafka topic k v =
      ProducerRecord topic UnassignedPartition encodedKey encodedValue
    where
      encodedValue = asKey v
      encodedKey   = asKey k

instance FromKafka PoolId ConfirmedPoolEvent where
  fromKafka consumerRecord =
      keyMaybe >>= (\key -> valueMaybe <&> (key,))
    where
      keyMaybe   = (fromKey $ crKey consumerRecord) :: Maybe PoolId
      valueMaybe = (fromKey $ crValue consumerRecord) :: Maybe ConfirmedPoolEvent

asKey :: (ToJSON a) => a -> Maybe ByteString.ByteString
asKey = Just . BS.toStrict . encode

fromKey :: (FromJSON a) => Maybe ByteString.ByteString -> Maybe a
fromKey (Just bs) = (decode . BS.fromStrict) bs
fromKey _         = Nothing