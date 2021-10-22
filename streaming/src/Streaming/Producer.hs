module Streaming.Producer where

import           RIO
import           Data.Either.Combinators
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Trans.Resource
import qualified Streamly.Prelude             as S
import           Kafka.Producer

import Streaming.Config
import Streaming.Class

data Producer f k v = Producer
  { produce :: S.SerialT f (k, v) -> f ()
  }

mkKafkaProducer
  :: (MonadError KafkaError f, S.MonadAsync f, ToKafka k v)
  => KafkaProducerConfig
  -> TopicName
  -> ResourceT f (Producer f k v)
mkKafkaProducer conf topic = do
  let
    props              = mkProducerProps conf
    spawn              = newProducer props
    close (Right prod) = closeProducer prod
    close _            = pure ()

  (_, prodTry) <- allocate spawn close
  prod         <- eitherToError prodTry

  pure $ Producer (produce' prod topic)

produce'
  :: (MonadError KafkaError f, S.MonadAsync f, ToKafka k v)
  => KafkaProducer
  -> TopicName
  -> S.SerialT f (k, v)
  -> f ()
produce' prod topic upstream =
    upstream
  & S.mapM (\(k, v) -> produceMessage prod (toKafka topic k v))
  & S.map (maybeToLeft ())
  & S.mapM eitherToError
  & S.drain

mkProducerProps :: KafkaProducerConfig -> ProducerProperties
mkProducerProps KafkaProducerConfig{..} =
     brokersList (fmap BrokerAddress producerBrokers)
  <> sendTimeout (Timeout producerTimeout)
  <> logLevel KafkaLogDebug


