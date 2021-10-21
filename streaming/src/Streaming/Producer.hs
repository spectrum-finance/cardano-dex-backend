module Streaming.Producer where

import RIO
import Data.Either.Combinators
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class
import Streamly.Prelude             (SerialT)
import Kafka.Producer

import Streaming.Config

data Producer f a = Producer
  { produce :: SerialT f a -> f ()
  }

mkKafkaProducer
  :: (MonadError KafkaError f, MonadIO f)
  => KafkaProducerConfig
  -> ResourceT f (Producer f a)
mkKafkaProducer conf = do
  let
    props                  = mkProducerProps conf
    spawnProd              = newProducer props
    closeProd (Right prod) = closeProd $ Right prod
    closeProd _            = pure ()

  (rkey, prodTry) <- allocate spawnProd closeProd
  prod            <- eitherToError prodTry

  pure $ Producer (produce' prod)

produce' :: KafkaProducer -> SerialT f a -> f ()
produce' prod upstream = undefined

mkProducerProps :: KafkaProducerConfig -> ProducerProperties
mkProducerProps KafkaProducerConfig{..} =
     brokersList (fmap BrokerAddress brokers)
  <> sendTimeout (Timeout timeout)
  <> logLevel KafkaLogDebug


