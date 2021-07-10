module Executor.KafkaClient (runKafka) where

import Control.Exception as C (bracket) 
import Kafka.Consumer
import RIO
import Prelude (print)
import qualified Streamly.Prelude as S
import RIO.ByteString as BS
import Data.Aeson
import RIO.ByteString.Lazy as LBS
import Dex.Models
import Executor.Models.Settings
import Data.Monoid

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

pollMessageF :: KafkaConsumerSettings -> KafkaConsumer -> IO (Maybe ParsedOperation)
pollMessageF settings consumer = do
    msg <- pollMessage consumer (Timeout $ pollRateS settings)
    _   <- print msg
    let parsedMsg = parseMessage msg
    err <- commitAllOffsets OffsetCommit consumer
    _   <- print $ "Offsets: " <> maybe "Committed." show err
    pure $ parsedMsg

parseMessage :: Either e (ConsumerRecord k (Maybe BS.ByteString)) -> Maybe ParsedOperation
parseMessage x = case x of Right xv -> crValue xv >>= (\msg -> decodeTest msg)
                           _ -> Nothing

decodeTest :: BS.ByteString -> Maybe ParsedOperation
decodeTest testData =
    let lazyStr = LBS.fromStrict testData
        decodedA = decode lazyStr :: Maybe SwapOpData
        decodeB = decode lazyStr :: Maybe DepositOpData
        decodeC = decode lazyStr :: Maybe RedeemOpData
    in 
        if (isJust decodedA) then ParsedOperation <$> (SwapOperation <$> decodedA)
        else if (isJust decodeB) then ParsedOperation <$> (DepositOperation <$> decodeB)
        else if (isJust decodeC) then ParsedOperation <$> (RedeemOperation <$> decodeC)
        else Nothing
    -- in ParsedOperation <$> getFirst (First decodedA <> First decodeB <> First decodeC)