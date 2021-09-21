module Tracker.Wirings.WiringApp
    ( runApp
    ) where

import RIO
import Tracker.Services.TxOutsProcessor
import Tracker.Services.KafkaService
import Tracker.Services.HttpReqService
import Tracker.Services.SettingsReader
import Dex.Processor

runApp :: IO ()
runApp = undefined -- do
--    let settingsReader = mkSettingsReader
--    appSettings <- read settingsReader
--    kafkaService <- mkKafkaService
--    let txOutsProcessor = mkTxOutsProcessor mkProcessorService kafkaService mkHttpReqService
--    runRIO appSettings $ do (run txOutsProcessor)