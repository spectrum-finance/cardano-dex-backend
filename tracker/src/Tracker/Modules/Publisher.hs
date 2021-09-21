module Tracker.Modules.Publisher where

import Dex.Models
import Tracker.Services.KafkaService
import RIO

data Publisher = Publisher {
	publishPool :: [Pool] -> IO (),
	publishParsedOp :: [ParsedOperation] -> IO ()
}

mkPublisher :: KafkaService -> IO Publisher
mkPublisher kafka = pure $ Publisher (publishPool' kafka) (publishParsedOp' kafka)

publishPool' :: KafkaService -> [Pool] -> IO ()
publishPool' KafkaService{..} = sendAmm

publishParsedOp' :: KafkaService -> [ParsedOperation] -> IO ()
publishParsedOp' KafkaService{..} = sendProxy