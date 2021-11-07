module Executor.Wirings.WiringApp
  ( wire
  ) where

import Executor.Services.ConfigReader
import Executor.Models.Config 
import Executor.Programs.Processor
import Executor.Services.OrdersExecutor
import Executor.Services.PoolsResolver

import Streaming.Consumer

import RIO
import Control.Monad.Trans.Resource

import ErgoDex.Amm.PoolActions

wire :: IO ()
wire = runResourceT $ do
  AppConfig {..} <- lift $ read mkConfigReader
  consumer       <- mkKafkaConsumer kafkaConfig [topicId]
  let
    poolsResolver  = mkPoolsResolver poolsResolverConfig
    poolAction     = mkPoolActions (mkPubKeyHash $ pubKeyHash paymentConfig)
    ordersExecutor = mkOrdersExecutor poolAction poolsResolver
    processor      = mkProcessor ordersExecutor consumer
  lift $ run processor