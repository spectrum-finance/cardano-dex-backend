module Executor.Wirings.WiringApp
    ( runApp
    ) where

import Executor.Clients.PoolsResolverClient
import Executor.Services.SettingsReader
import Executor.Services.Processor
import Executor.Services.KafkaService
import Executor.Services.BashService
import Executor.Models.Settings 

import RIO
import Data.Text.Encoding as Data

import ErgoDex.Amm.PoolActions

import PlutusTx.Builtins.Internal
import Plutus.V1.Ledger.Crypto

runApp :: IO ()
runApp = do
    let settingsReader = mkSettingsReader
    appSettings <- read settingsReader
    runRIO appSettings $ do
        let httpClient = mkPoolsResolverClient (getHttpSettings appSettings)
            bashService = mkBashService $ paymentSettings appSettings
            poolAction = mkPoolActions (PubKeyHash $ BuiltinByteString $ Data.encodeUtf8 $ pubKeyHash $ paymentSettings appSettings)
            processor = mkProcessor poolAction bashService httpClient
            kafkaClient = mkKafkaService processor
        runKafka kafkaClient