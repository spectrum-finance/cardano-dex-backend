module Executor.Wirings.WiringApp
  ( wire
  ) where

import Executor.Services.ConfigReader
import Executor.Models.Config 
import Executor.Programs.Processor
import Executor.Services.OrdersExecutor
import Executor.Services.PoolsResolver
import Explorer.Service

import Streaming.Consumer

import RIO
import Control.Monad.Trans.Resource

import SubmitAPI.Service
import ErgoDex.Amm.PoolActions
import WalletAPI.TrustStore
import WalletAPI.Vault
import NetworkAPI.Service

wire :: IO ()
wire = runResourceT $ do
  AppConfig {..} <- lift $ read mkConfigReader
  consumer       <- mkKafkaConsumer kafkaConfig [topicId]
  let
    poolsResolver  = mkPoolsResolver poolsResolverConfig
    explorer       = mkExplorer explorerConfig
    trustStore     = (mkTrustStore secretFile) :: TrustStore IO
  vault          <- lift $ mkVault explorer trustStore keyPass
  let
    network        = (mkNetwork nodeConfig explorer) :: Network IO
    submitService  = mkSubmitService network vault txAssemblyConfig
    poolAction     = mkPoolActions (mkPubKeyHash $ pubKeyHash paymentConfig)
    ordersExecutor = mkOrdersExecutor poolAction poolsResolver submitService
    processor      = mkProcessor ordersExecutor consumer
  lift $ run processor