module Streaming.Consumer where

import           RIO
import           Data.Either.Combinators
import           Control.Monad.Except
import           Control.Monad.Trans.Resource
import qualified Streamly.Prelude             as S
import           Kafka.Consumer
import           Data.Text

import Streaming.Config
import Streaming.Class
import Streaming.Types

data Consumer f k v = Consumer
  { upstream :: S.SerialT f (k, v)
  }

mkKafkaConsumer
  :: (MonadError KafkaError f, S.MonadAsync f, FromKafka k v)
  => KafkaConsumerConfig
  -> [TopicId]
  -> ResourceT f (Consumer f k v)
mkKafkaConsumer conf subs = do
  let
    props   = mkConsumerProps conf
    subConf =
         topics (fmap (TopicName . pack . unTopicId) subs)
      <> offsetReset Earliest

    spawn            = newConsumer props subConf
    close (Right cr) = closeConsumer cr <&> maybe (Right ()) Left
    close (Left err) = pure $ Left err

  (_, crTry) <- allocate spawn (void . close)
  cr         <- eitherToError crTry

  pure $ Consumer (upstream' cr undefined undefined)

upstream'
  :: (MonadError KafkaError f, S.MonadAsync f, FromKafka k v)
  => KafkaConsumer
  -> Timeout
  -> BatchSize
  -> S.SerialT f (k, v)
upstream' consumer timeout batchSize =
    readUpstream >>= unNone
  where
    unNone r     = maybe S.nil pure r
    readUpstream =
        S.repeatM (pollMessageBatch consumer timeout batchSize)
      & S.fromAsync >>= S.fromList
      & S.mapM eitherToError
      & S.map fromKafka

mkConsumerProps :: KafkaConsumerConfig -> ConsumerProperties
mkConsumerProps KafkaConsumerConfig{..} =
     brokersList (fmap BrokerAddress consumerBrokers)
  <> groupId (ConsumerGroupId consumerGroupId)
  <> noAutoCommit
  <> logLevel KafkaLogInfo
