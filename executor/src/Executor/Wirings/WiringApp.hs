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
import Executor.Clients.PoolsResolverClient

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
import System.Logging.Hlog

wire :: IO ()
wire = runResourceT $ do
  AppConfig {..} <- lift $ read mkConfigReader
  consumer             <- mkKafkaConsumer kafkaConfig [topicId]
  loggingMaker         <- makeLogging @(ResourceT IO) @IO loggingConfig
  poolsResolverClient  <- mkPoolsResolverClient poolsResolverConfig loggingMaker
  poolsResolver        <- mkPoolsResolver poolsResolverConfig poolsResolverClient loggingMaker
  let
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
  ordersExecutor <- mkOrdersExecutor poolAction loggingMaker poolsResolver transactions
  processor      <- mkProcessor ordersExecutor loggingMaker consumer
  lift $ run processor
