module Streaming.Consumer where

import           RIO
import           Control.Monad.Trans.Resource
import qualified Streamly.Prelude             as S
import           Kafka.Consumer
import           Data.Text
import           Data.Bifunctor               as Either
import           GHC.Natural                  as Natural

import Streaming.Config
import Streaming.Class
import Streaming.Types

import Core.Throw.Combinators

data Consumer f k v = Consumer
  { upstream :: S.SerialT f (k, v)
  }

mkKafkaConsumer
  :: (MonadThrow f, S.MonadAsync f, FromKafka k v)
  => KafkaConsumerConfig
  -> [TopicId]
  -> ResourceT f (Consumer f k v)
mkKafkaConsumer conf@KafkaConsumerConfig{..} subs = do
  let
    props   = mkConsumerProps conf
    subConf =
         topics (fmap (TopicName . pack . unTopicId) subs)
      <> offsetReset Earliest

    spawn            = newConsumer props subConf
    close (Right cr) = closeConsumer cr <&> maybe (Right ()) Left
    close (Left err) = pure $ Left err

  (_, crTry) <- allocate spawn (void . close)
  cr         <- throwEither crTry

  pure $ Consumer (upstream' cr (Timeout $ Natural.naturalToInt consumerTimeout) (BatchSize $ Natural.naturalToInt consumerBatchSize))

upstream'
  :: (MonadThrow f, S.MonadAsync f, FromKafka k v)
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
      & S.map (Either.first (const ConsumerException))
      & S.mapM throwEither
      & S.map fromKafka
      & S.mapM (commitAllOffsets OffsetCommit consumer $>)

mkConsumerProps :: KafkaConsumerConfig -> ConsumerProperties
mkConsumerProps KafkaConsumerConfig{..} =
     brokersList (fmap BrokerAddress consumerBrokers)
  <> extraProp "max.poll.interval.ms" (pack . show $ maxPollIntervalMs)
  <> groupId (ConsumerGroupId consumerGroupId)
  <> noAutoCommit
  <> logLevel KafkaLogInfo
