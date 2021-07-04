module Resolver.KafkaClient (runKafka) where

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

consumerProps :: KafkaConsumerSettings -> ConsumerProperties
consumerProps settings = brokersList (brokerListS settings)
             <> groupId (groupIdS settings)
             <> noAutoCommit
             <> logLevel KafkaLogInfo

consumerSub :: KafkaConsumerSettings -> Subscription
consumerSub settings = topics (topicsListS settings)
           <> offsetReset Earliest

runKafka :: HasKafkaConsumerSettings env => RIO env ()
runKafka = do
    settings <- view kafkaSettingsL
    runKafka' settings

runKafka' :: KafkaConsumerSettings -> RIO env ()
runKafka' settings = 
    liftIO $ do
    _   <- print "Running kafka stream..."
    C.bracket mkConsumer clConsumer runHandler
    where
      mkConsumer = newConsumer (consumerProps settings) (consumerSub settings)
      clConsumer (Left err) = return (Left err)
      clConsumer (Right kc) = maybe (Right ()) Left <$> closeConsumer kc
      runHandler (Left err) = print err >> pure ()
      runHandler (Right kc) = runF settings kc

-- -------------------------------------------------------------------

runF :: KafkaConsumerSettings -> KafkaConsumer -> IO ()
runF settings consumer = S.drain $ S.repeatM $ pollMessageF settings consumer

pollMessageF :: KafkaConsumerSettings -> KafkaConsumer -> IO (Maybe Pool)
pollMessageF settings consumer = do
    msg <- pollMessage consumer (Timeout $ pollRateS settings)
    _   <- print msg
    let parsedMsg = parseMessage msg
    _   <- print parsedMsg
    err <- commitAllOffsets OffsetCommit consumer
    _   <- print $ "Offsets: " <> maybe "Committed." show err
    pure parsedMsg

parseMessage :: Either e (ConsumerRecord k (Maybe BS.ByteString)) -> Maybe Pool
parseMessage x = case x of Right xv -> crValue xv >>= (\msg -> (decode $ LBS.fromStrict msg) :: Maybe Pool)
                           _ -> Nothing
