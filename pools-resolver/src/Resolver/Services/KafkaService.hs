module Resolver.Services.KafkaService
    ( KafkaService(..)
    , mkKafkaService
    ) where

import Control.Exception as C (bracket) 
import Kafka.Consumer
import RIO
import Prelude (print)
import qualified Streamly.Prelude as S
import RIO.ByteString as BS
import Data.Aeson
import RIO.ByteString.Lazy as LBS
import Resolver.Models.CfmmPool
import Dex.Models
import Resolver.Models.AppSettings
import Resolver.Repositories.PoolRepository
import RIO.List as List
import GHC.Natural

data KafkaService env = KafkaService
    { runKafka :: HasKafkaConsumerSettings env => RIO env () 
    }

mkKafkaService :: PoolRepository -> KafkaService env
mkKafkaService p = KafkaService $ runKafka' p

runKafka' :: HasKafkaConsumerSettings env => PoolRepository -> RIO env ()
runKafka' p = do
    settings <- view kafkaSettingsL
    mkKafka p settings

----------------------------------------------------------------------

consumerProps :: KafkaConsumerSettings -> ConsumerProperties
consumerProps settings = brokersList (List.map BrokerAddress (getBrokerList settings))
             <> groupId (ConsumerGroupId $ getGroupId settings)
             <> noAutoCommit
             <> logLevel KafkaLogInfo

consumerSub :: KafkaConsumerSettings -> Subscription
consumerSub settings = topics (List.map TopicName (getTopicsList settings))
           <> offsetReset Earliest

mkKafka :: PoolRepository -> KafkaConsumerSettings -> RIO env ()
mkKafka p settings = 
    liftIO $ do
    _   <- print "Running kafka stream..."
    C.bracket mkConsumer clConsumer runHandler
    where
      mkConsumer = newConsumer (consumerProps settings) (consumerSub settings)
      clConsumer (Left err) = return (Left err)
      clConsumer (Right kc) = maybe (Right ()) Left <$> closeConsumer kc
      runHandler (Left err) = print err >> pure ()
      runHandler (Right kc) = runF p settings kc

runF :: PoolRepository -> KafkaConsumerSettings -> KafkaConsumer -> IO ()
runF p settings consumer = S.drain $ S.repeatM $ pollMessageF p settings consumer

pollMessageF :: PoolRepository -> KafkaConsumerSettings -> KafkaConsumer -> IO ()
pollMessageF PoolRepository{..} settings consumer = do
    msgs <- pollMessageBatch consumer (Timeout $ fromIntegral . naturalToInteger $ getPollRate settings) (BatchSize $ fromIntegral . naturalToInteger $ getBatchSize settings)
    _   <- print msgs
    let parsedMsg = fmap parseMessage msgs
        confirmed = fmap (ConfirmedPool <$>) parsedMsg & catMaybes
    _ <- traverse putConfirmed confirmed
    err <- commitAllOffsets OffsetCommit consumer
    print $ "Offsets: " <> maybe "Committed." show err

parseMessage :: Either e (ConsumerRecord k (Maybe BS.ByteString)) -> Maybe Pool
parseMessage x = case x of Right xv -> crValue xv >>= (\msg -> (decode $ LBS.fromStrict msg) :: Maybe Pool)
                           _ -> Nothing
