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
import Tracker.Services.Logger as Log

import SubmitAPI.Service
import ErgoDex.Amm.PoolActions
import WalletAPI.TrustStore
import WalletAPI.Vault
import NetworkAPI.Service

wire :: IO ()
wire = runResourceT $ do
  AppConfig {..} <- lift $ read mkConfigReader
  _ <- lift $ Log.log "test1"
  consumer       <- mkKafkaConsumer kafkaConfig [topicId]
  _ <- lift $ Log.log "test2"
  let
    poolsResolver  = mkPoolsResolver poolsResolverConfig
    explorer       = mkExplorer explorerConfig
    trustStore     = (mkTrustStore $ SecretFile "/Users/aleksandr/IdeaProjects/cardano-dex-backend/executor/resources/keys.txt") :: TrustStore IO
  _ <- lift $ Log.log "test3"
  vault          <- lift $ mkVault explorer trustStore keyPass
  _ <- lift $ Log.log "test4"
  let
    network        = (mkNetwork nodeConfig explorer) :: Network IO
    submitService  = mkSubmitService network vault txAssemblyConfig
    poolAction     = mkPoolActions (mkPubKeyHash $ pubKeyHash paymentConfig)
    ordersExecutor = mkOrdersExecutor poolAction poolsResolver submitService
    processor      = mkProcessor ordersExecutor consumer
  _ <- lift $ Log.log "test5"
  lift $ run processor