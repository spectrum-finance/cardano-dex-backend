module Tracker.KafkaClient 
    ( KafkaProducerClient(..)
    , mkKafkaProducerClient
    ) where

import Plutus.V1.Ledger.Tx ( TxOut(..) )
import RIO
import qualified RIO.List as L
import Data.Aeson
import Prelude (print)
import Kafka.Producer
import Dex.Models (PoolId)
import RIO.ByteString.Lazy as BS
import Tracker.Models.AppSettings (KafkaProducerSettings(..), HasKafkaProducerSettings(..))
import Dex.Models

data KafkaProducerClient env = KafkaProducerClient
    { sendProxy :: HasKafkaProducerSettings env => [Pool] -> RIO env ()
    , sendAmm :: HasKafkaProducerSettings env => [ParsedOperation] -> RIO env ()
    }

mkKafkaProducerClient :: KafkaProducerClient env
mkKafkaProducerClient = KafkaProducerClient sendProxy' sendAmm'

-- ---------- Utils functions ------------
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


-- ---------- Module api -----------------

-- Check if new producer creates on each call

-- Send unspent boxes with proxy contract to kafka
sendProxy' :: HasKafkaProducerSettings env => [Pool] -> RIO env ()
sendProxy' txOuts = do
    settings <- view kafkaProducerSettingsL
    liftIO $ runProducerLocal (brokersListS settings) (sendMessages $ formProducerRecord (proxyMsgKey settings) (proxyTopic settings) txOuts)

encodeOperation :: ParsedOperation -> BS.ByteString
encodeOperation (ParsedOperation op) = 
    case op of
        x@ (SwapOperation swapData) -> encode swapData
        x@ (DepositOperation depositData) -> encode depositData
        x@ (RedeemOperation redeemData) -> encode redeemData  

-- Send unspent boxes with amm contract to kafka
sendAmm' :: HasKafkaProducerSettings env => [ParsedOperation] -> RIO env ()
sendAmm' txOuts = do
    settings <- view kafkaProducerSettingsL
    liftIO $ runProducerLocal (brokersListS settings) (sendMessages $ formProducerRecordOperation (ammMsgKey settings) (ammTopic settings) (RIO.map encodeOperation txOuts)) 
