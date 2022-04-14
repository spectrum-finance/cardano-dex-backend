module Executor.Wirings.WiringApp
  ( wire
  ) where

import Executor.Services.ConfigReader
import Common.Cardano.Interop
import Executor.Models.Config
import qualified Debug.Trace as D
import Executor.Programs.Processor
import Executor.Services.OrdersExecutor
import Executor.Services.PoolsResolver

import Streaming.Consumer

import RIO
import Control.Monad.Trans.Resource

import qualified Cardano.Api as C
import           Ledger      (PaymentPubKeyHash(..))

import NetworkAPI.Types
import NetworkAPI.Node.Service
import ErgoDex.Amm.PoolActions
import WalletAPI.TrustStore
import WalletAPI.Vault
import WalletAPI.Utxos
import NetworkAPI.Service
import Explorer.Service
import SubmitAPI.Config
import SubmitAPI.Service

wire :: IO ()
wire = runResourceT $ do
  AppConfig {..} <- lift $ read mkConfigReader
  consumer       <- mkKafkaConsumer kafkaConfig [topicId]
  let
    poolsResolver  = mkPoolsResolver poolsResolverConfig
    explorer       = mkExplorer explorerConfig
    trustStore     = mkTrustStore @_ @C.PaymentKey C.AsPaymentKey secretFile
    vault          = mkVault trustStore keyPass
  walletOutputs <- lift $ mkWalletOutputs' explorer vault
  executorPkh   <- lift $ fmap fromCardanoPaymentKeyHash (getPaymentKeyHash vault)
  let
    epochSlots     = C.CardanoModeParams $ C.EpochSlots 21600
    networkId      = C.Testnet (C.NetworkMagic 1097911063)
    sockPath       = SocketPath (nodeSocketPath nodeSocketConfig)
    network        = mkNetwork C.AlonzoEra epochSlots networkId sockPath
    transactions   = mkTransactions network networkId walletOutputs vault txAssemblyConfig
    poolAction     = mkPoolActions (PaymentPubKeyHash $ executorPkh)
    ordersExecutor = mkOrdersExecutor poolAction poolsResolver transactions
    processor      = mkProcessor ordersExecutor consumer
  lift $ run processor
