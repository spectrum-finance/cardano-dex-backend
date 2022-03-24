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

import qualified Cardano.Api as C
import           Ledger      (PaymentPubKeyHash(..))

import ErgoDex.Amm.PoolActions
import WalletAPI.TrustStore
import WalletAPI.Vault
import WalletAPI.Utxos
import NetworkAPI.Service
import Explorer.Service
import SubmitAPI.Service

wire :: IO ()
wire = runResourceT $ do
  AppConfig {..} <- lift $ read mkConfigReader
  consumer       <- mkKafkaConsumer kafkaConfig [topicId]
  let
    poolsResolver  = mkPoolsResolver poolsResolverConfig
    explorer       = mkExplorer explorerConfig
    trustStore     = mkTrustStore @_ @C.PaymentKey C.AsPaymentKey src
      where src = SecretFile "executor/resources/keys.txt"
    vault = mkVault trustStore keyPass
  walletOutputs <- lift $ mkWalletOutputs' explorer vault
  let
    network        = mkNetwork nodeConfig explorer
    transactions   = mkTransactions network walletOutputs vault txAssemblyConfig
    poolAction     = mkPoolActions (PaymentPubKeyHash $ mkPubKeyHash $ pubKeyHash paymentConfig) Nothing
    ordersExecutor = mkOrdersExecutor poolAction poolsResolver transactions
    processor      = mkProcessor ordersExecutor consumer
  lift $ run processor