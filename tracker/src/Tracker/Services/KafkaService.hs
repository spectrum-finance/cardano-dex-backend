module Tracker.Services.KafkaService 
    ( KafkaService(..)
    , mkKafkaService
    ) where

import RIO
import qualified RIO.List as L
import Data.Aeson
import Prelude (print)
import Kafka.Producer
import RIO.ByteString.Lazy as BS
import Tracker.Models.AppSettings (KafkaProducerSettings(..), HasKafkaProducerSettings(..))
import Dex.Models
import RIO.List as List
import RIO.Text as Text

data KafkaService env = KafkaService
    { sendProxy :: HasKafkaProducerSettings env => [ParsedOperation] -> RIO env ()
    , sendAmm :: HasKafkaProducerSettings env => [Pool] -> RIO env ()
    }

mkKafkaService :: IO (KafkaService env)
mkKafkaService = do
    let service = KafkaService sendProxy' sendAmm'
    _ <- print "Kafka service was initialized successfully"
    pure service

sendProxy' :: HasKafkaProducerSettings env => [ParsedOperation] -> RIO env ()
sendProxy' parsedOps = do
    settings <- view kafkaProducerSettingsL
    liftIO $ runProducerLocal 
        (List.map BrokerAddress (getBrokersList settings)) 
        (sendMessages $ formProducerRecordOperation 
            (Text.encodeUtf8 $ getProxyMsgKey settings) 
            (TopicName $ getProxyTopic settings) 
            (RIO.map encodeOperation parsedOps)
        )

sendAmm' :: HasKafkaProducerSettings env => [Pool] -> RIO env ()
sendAmm' txOuts = do
    settings <- view kafkaProducerSettingsL
    liftIO $ runProducerLocal (List.map BrokerAddress (getBrokersList settings)) (sendMessages $ formProducerRecord (Text.encodeUtf8 $ getAmmMsgKey settings) (TopicName $ getAmmTopic settings) txOuts)

-------------------------------------------------------------------------------------

myLogCallback :: KafkaLogLevel -> String -> String -> IO ()
myLogCallback level facility message = print $ show level <> "|" <> facility <> "|" <> message

producerProps :: [BrokerAddress] -> ProducerProperties
producerProps ips = brokersList ips
                        <> logLevel KafkaLogDebug
                        <> setCallback (logCallback myLogCallback)

runProducerLocal :: [BrokerAddress] -> (KafkaProducer -> IO (Either KafkaError ())) -> IO ()
runProducerLocal ips f =
    bracket mkProducer clProducer runHandler >>= print
    where
      mkProducer = newProducer $ producerProps ips
      clProducer (Left err)   = print err >> return ()
      clProducer (Right prod) = closeProducer prod
      runHandler (Left err)   = return $ Left err
      runHandler (Right prod) = f prod

sendMessages :: [ProducerRecord] -> KafkaProducer -> IO (Either KafkaError ())
sendMessages msg prod = do
    err <- produceMessageBatch prod msg
    _   <- print err
    return $ Right ()

mkMessage :: TopicName -> Maybe RIO.ByteString -> Maybe RIO.ByteString -> ProducerRecord
mkMessage t k v = ProducerRecord
                  { prTopic = t
                  , prPartition = UnassignedPartition
                  , prKey = k
                  , prValue = v
                  }

formProducerRecord :: (ToJSON a) => RIO.ByteString -> TopicName -> [a] -> [ProducerRecord]
formProducerRecord s t = L.map (mkMessage t (Just s) . Just . BS.toStrict . encode)

formProducerRecordOperation :: RIO.ByteString -> TopicName -> [BS.ByteString] -> [ProducerRecord]
formProducerRecordOperation s t = L.map (mkMessage t (Just s) . Just . BS.toStrict)

encodeOperation :: ParsedOperation -> BS.ByteString
encodeOperation (ParsedOperation op) = 
    case op of
        (SwapOperation swapData) -> encode swapData
        (DepositOperation depositData) -> encode depositData
        (RedeemOperation redeemData) -> encode redeemData  