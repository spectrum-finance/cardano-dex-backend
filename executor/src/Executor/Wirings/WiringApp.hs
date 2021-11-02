module Executor.Wirings.WiringApp
    ( wire
    ) where

import Executor.Services.ConfigReader
import Executor.Models.Settings 
import Executor.Programs.Processor
import Executor.Services.OrdersExecutor
import Executor.Services.PoolsResolver

import Streaming.Consumer

import RIO
import Data.Text.Encoding           as Data
import Control.Monad.Trans.Resource

import ErgoDex.Amm.PoolActions

import PlutusTx.Builtins.Internal
import Plutus.V1.Ledger.Crypto

wire :: IO ()
wire = runResourceT $ do
  AppConfig {..} <- lift $ read mkConfigReader
  consumer       <- mkKafkaConsumer kafkaConfig [topicId]
  let
    poolsResolver  = mkPoolsResolver poolsResolverConfig
    poolAction     = mkPoolActions (PubKeyHash $ BuiltinByteString $ Data.encodeUtf8 $ pubKeyHash paymentConfig)
    ordersExecutor = mkOrdersExecutor poolAction poolsResolver
    processor      = mkProcessor ordersExecutor consumer
  lift $ run processor