module Tracker.Models.AppSettings 
    ( HttpSettings(..)
    , BlockRequestSettings(..)
    , KafkaProducerSettings(..)
    , AppSettings(..)
    , HasHttpSettings(..)
    , HasBlockRequestSettings(..)
    , HasKafkaProducerSettings(..)
    , HasAppSettings(..)
    ) where

import RIO ( Show, Int, id, lens, String, Lens' )
import RIO.Text
import RIO.ByteString as BS
import Kafka.Producer

data HttpSettings = HttpSettings
    { hostS :: String
    , portS :: Int
    } deriving (Show)

newtype BlockRequestSettings = BlockRequestSettings
    { period :: Int } deriving (Show)

data AppSettings = AppSettings
    { httpSettings :: HttpSettings
    , blockRequestSettings :: BlockRequestSettings
    , kafkaProducerSettings :: KafkaProducerSettings
    } deriving (Show)

data KafkaProducerSettings = KafkaProducerSettings 
  { ammTopic :: TopicName
  , proxyTopic :: TopicName
  , brokersListS :: [BrokerAddress]
  , proxyMsgKey :: BS.ByteString
  , ammMsgKey :: BS.ByteString
  } deriving (Show)

class HasHttpSettings env where
  httpSettingsL :: Lens' env HttpSettings
instance HasHttpSettings HttpSettings where
  httpSettingsL = id
instance HasHttpSettings AppSettings where
  httpSettingsL = lens httpSettings (\x y -> x { httpSettings = y })

class HasBlockRequestSettings env where
  blockRequestSettingsL :: Lens' env BlockRequestSettings
instance HasBlockRequestSettings BlockRequestSettings where
  blockRequestSettingsL = id
instance HasBlockRequestSettings AppSettings where
  blockRequestSettingsL = lens blockRequestSettings (\x y -> x { blockRequestSettings = y })

class HasAppSettings env where
  appSettingsL :: Lens' env AppSettings
instance HasAppSettings AppSettings where
  appSettingsL = id

class HasKafkaProducerSettings env where
  kafkaProducerSettingsL :: Lens' env KafkaProducerSettings
instance HasKafkaProducerSettings KafkaProducerSettings where
  kafkaProducerSettingsL = id
instance HasKafkaProducerSettings AppSettings where
  kafkaProducerSettingsL = lens kafkaProducerSettings (\x y -> x { kafkaProducerSettings = y })