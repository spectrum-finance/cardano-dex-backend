module Streaming.Producer where

import           RIO
import           Data.Either.Combinators
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.Trans.Resource
import qualified Streamly.Prelude             as S
import           Kafka.Producer
import           GHC.Natural                  as Natural

import Streaming.Config
import Streaming.Class
import Data.Bifunctor as Either
import Streaming.Types

data Producer f k v = Producer
  { produce :: S.SerialT f (k, v) -> f ()
  }
 
mkKafkaProducer
  :: (MonadThrow f, S.MonadAsync f, ToKafka k v)
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
  prod         <- throwEither prodTry

  pure $ Producer (produce' prod topic)

produce'
  :: (MonadThrow f, S.MonadAsync f, ToKafka k v)
  => KafkaProducer
  -> TopicName
  -> S.SerialT f (k, v)
  -> f ()
produce' prod topic upstream =
    upstream
  & S.mapM (\(k, v) -> produceMessage prod (toKafka topic k v))
  & S.map (maybeToLeft ())
  & S.map (Either.first (const ProducerExecption))
  & S.mapM throwEither
  & S.drain

throwEither :: (MonadThrow f, Exception e) => Either e r -> f r
throwEither (Left err)    = throwM err
throwEither (Right value) = pure value

mkProducerProps :: KafkaProducerConfig -> ProducerProperties
mkProducerProps KafkaProducerConfig{..} =
     brokersList (fmap BrokerAddress producerBrokers)
  <> sendTimeout (Timeout $ Natural.naturalToInt producerTimeout)
  <> logLevel KafkaLogDebug