module Resolver.KafkaClient where

import Control.Exception as C (bracket) 
import Kafka.Consumer
import RIO
import Prelude (print)

-- Global consumer properties
consumerProps :: ConsumerProperties
consumerProps = brokersList ["0.0.0.0:9092"]
             <> groupId "consumer_example_group"
             <> noAutoCommit
             <> logLevel KafkaLogInfo

-- Subscription to topics
consumerSub :: Subscription
consumerSub = topics ["amm-topic-1"]
           <> offsetReset Earliest

-- Running an example
runConsumerExample :: IO ()
runConsumerExample = do
    res <- C.bracket mkConsumer clConsumer runHandler
    print res
    where
      mkConsumer = newConsumer consumerProps consumerSub
      clConsumer (Left err) = return (Left err)
      clConsumer (Right kc) = maybe (Right ()) Left <$> closeConsumer kc
      runHandler (Left err) = return (Left err)
      runHandler (Right kc) = processMessages kc

-------------------------------------------------------------------
processMessages :: KafkaConsumer -> IO (Either KafkaError ())
processMessages kafka = do
    replicateM_ 1000 $ do
      msg <- pollMessageBatch kafka (Timeout 1000) 2
      print $ "Message: " <> show msg
      err <- commitAllOffsets OffsetCommit kafka
      print $ "Offsets: " <> maybe "Committed." show err
    return $ Right ()