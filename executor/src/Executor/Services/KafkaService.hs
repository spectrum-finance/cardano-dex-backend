module Executor.Services.KafkaService
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
import Dex.Models
import Executor.Models.Settings
import Executor.Services.Processor

data KafkaService env = KafkaService
    { runKafka :: HasKafkaConsumerSettings env => RIO env ()
    }

mkKafkaService :: Processor -> KafkaService env
mkKafkaService p = KafkaService $ runKafka' p

runKafka' :: HasKafkaConsumerSettings env => Processor -> RIO env ()
runKafka' p = do
    settings <- view kafkaSettingsL
    mkKafka p settings

-------------------------------------------------------------------------------------

consumerProps :: KafkaConsumerSettings -> ConsumerProperties
consumerProps settings = brokersList (getBrokerList settings)
             <> groupId (getGroupId settings)
             <> noAutoCommit
             <> logLevel KafkaLogInfo

consumerSub :: KafkaConsumerSettings -> Subscription
consumerSub settings = topics (getTopicsList settings)
           <> offsetReset Earliest

mkKafka :: Processor -> KafkaConsumerSettings -> RIO env ()
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

runF :: Processor -> KafkaConsumerSettings -> KafkaConsumer -> IO ()
runF p settings consumer = S.drain $ S.repeatM $ pollMessageF p settings consumer

pollMessageF :: Processor -> KafkaConsumerSettings -> KafkaConsumer -> IO ()
pollMessageF Processor{..} settings consumer = do
    msgs <- pollMessageBatch consumer (Timeout $ getPollRate settings) (BatchSize $ getBatchSize settings)
    _   <- print msgs
    let parsedMsgs = fmap parseMessage msgs & catMaybes
    traverse process parsedMsgs
    err <- commitAllOffsets OffsetCommit consumer
    print $ "Offsets: " <> maybe "Committed." show err

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
