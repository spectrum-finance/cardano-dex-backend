{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Gen.ServicesGen where

import RIO
import Streamly.Internal.Data.Stream.IsStream 
  ( SerialT )
import Data.Text 
  ( pack )
import qualified Data.Set as Set
import qualified Data.Map as Map

import Ledger.Tx.CardanoAPI 
  ( fromCardanoPaymentKeyHash, toCardanoAddressInEra )
import Ledger
  ( TxOutRef(..), PaymentPubKeyHash(..) )
import Ledger.Value 
  ( flattenValue, CurrencySymbol (..), TokenName (..) )
import Cardano.Api 
  ( SerialiseAddress (..) )
import qualified Cardano.Api as C

import System.Logging.Hlog
  ( makeLogging, MakeLogging (..), translateMakeLogging, Logging(..) )

import SubmitAPI.Service
  ( Transactions(..), mkTransactions )
import Explorer.Service
  ( Explorer(..) )
import WalletAPI.TrustStore 
  ( KeyPass(..), TrustStore(..) )
import Tests.Backlog.PersistenceMock 
  ( mkMockStorage )
import NetworkAPI.Service 
  ( CardanoNetwork(..) )
import ErgoDex.Amm.PoolActions 
  ( mkPoolActions, fetchValidatorsV1 )
import CardanoTx.Models 
  ( ChangeAddress(..), FullTxOut (..) )
import NetworkAPI.Types 
  ( SystemEnv(..) )
import Explorer.Models 
  ( Items(..), OutAsset (..) )
import Explorer.Types
  ( OutRef(OutRef)
  , TxHash(TxHash)
  , Addr(Addr)
  , Gix(Gix)
  , PolicyId(..)
  , AssetName(..)
  )
import WalletAPI.Utxos 
  ( WalletOutputs(..), mkWalletOutputs' )
import WalletAPI.Vault 
  ( mkVault, Vault (Vault, getPaymentKeyHash) )
import SubmitAPI.Config 
  ( TxAssemblyConfig(..), FeePolicy (..), CollateralPolicy (..), DefaultChangeAddress (..) )
import qualified Explorer.Models as Explorer

import Spectrum.Executor.PoolTracker.Service 
  ( PoolResolver(..) )
import Spectrum.Executor.OrdersExecutor.Process
  ( mkOrdersExecutor, OrdersExecutor )
import Spectrum.Executor.Backlog.Service 
  ( BacklogService (..), mkBacklogService' )
import Spectrum.Executor.Types 
  ( Pool )
import Spectrum.Executor.Backlog.Persistence.BacklogStore 
  ( BacklogStore(..) )
import Spectrum.Executor.Backlog.Config 
  ( BacklogServiceConfig(..) )
import Spectrum.Prelude.HigherKind 
  ( LiftK (..) )
import qualified Spectrum.Executor.Types as Spectrum

import Gen.ConstantsGen 
  ( signingKey
  , staticProtocolParams
  , TestEnv
  , testAddress
  , mkValidators
  , mapValidators
  , ValidatorInfo (..)
  , cfgForOnlyPendingOrders
  , mockNetworkId
  )
import Gen.ExceptionsGen 
  ( BadInputsUTxOException(..) )
import Gen.LoggingGen 
  ( mkMakeLogging, mkEmptyMakeLogging )

mkService :: forall f . (MonadIO f, MonadIO f, LiftK f f) => MakeLogging f f -> BacklogServiceConfig -> BacklogStore f -> f (BacklogService f)
mkService = mkBacklogService'

mkCardanoNetworkMockWithSubmitionError :: CardanoNetwork IO C.BabbageEra
mkCardanoNetworkMockWithSubmitionError = CardanoNetwork
  { NetworkAPI.Service.getSystemEnv = pure $ SystemEnv
    { pparams    = staticProtocolParams
    }
  , NetworkAPI.Service.submitTx     = \_ -> throwIO BadInputsUTxOException
  }

mkMockTrustStore :: (Applicative f) => TrustStore f C.PaymentKey
mkMockTrustStore = TrustStore
  { init     = \_ -> pure ()
  , readSK   = \_ -> pure signingKey
  , readVK   = pure $ C.getVerificationKey signingKey
  , isInitialized = pure True
  }

instance LiftK IO IO where
  liftK = id

mkBacklogService :: forall f. (MonadIO f) => f (BacklogService IO)
mkBacklogService = do
    mockStorage <- mkMockStorage
    liftIO $ mkService mkMakeLogging cfgForOnlyPendingOrders mockStorage

mkMockValletOutputs :: forall f. (MonadIO f, Applicative f) => FullTxOut -> WalletOutputs f
mkMockValletOutputs userOut = WalletOutputs
  { selectUtxos       =  \_ -> pure . Just $ Set.fromList [userOut]
  , selectUtxosStrict =  \_ -> pure . Just $ Set.fromList [userOut]
  }

mkMockPoolResolver :: (MonadIO f, Applicative f) => Pool -> PoolResolver f
mkMockPoolResolver pool = PoolResolver
  { resolvePool    = \_ -> pure (Just pool)
  , putPool        = \_ -> pure ()
  , invalidatePool = \_ -> pure ()
  }

makeMockTransactions :: Explorer IO -> IO (Transactions IO C.BabbageEra)
makeMockTransactions explorer = do
  let
    refScriptsMap = Map.empty
    cardanoNetwork = mkCardanoNetworkMockWithSubmitionError

    keyPass = KeyPass "testKeyPass"
    tsStore = mkMockTrustStore
    vault   = mkVault tsStore keyPass

    txAssemblyConfig = TxAssemblyConfig
      { feePolicy         = Balance
      , collateralPolicy  = Cover
      , deafultChangeAddr = DefaultChangeAddress . ChangeAddress $ testAddress
      }

  walletOutputs <- mkWalletOutputs' id mkEmptyMakeLogging explorer vault
  pure $ mkTransactions cardanoNetwork mockNetworkId refScriptsMap walletOutputs vault txAssemblyConfig

mkMockOrdersExecutor :: [Spectrum.Order] -> BacklogService IO -> PoolResolver IO -> Explorer IO -> IO (OrdersExecutor SerialT IO)
mkMockOrdersExecutor orders backlogService poolResolver explorer = do
  syncSem <- newQSem 1
  let
      keyPass = KeyPass "testKeyPass"
      tsStore = mkMockTrustStore
      vault   = mkVault tsStore keyPass

  transactions   <- makeMockTransactions explorer
  executorPkh    <- fmap fromCardanoPaymentKeyHash (getPaymentKeyHash vault)
  validators     <- fetchValidatorsV1
  let
    poolActions  = mkPoolActions (PaymentPubKeyHash executorPkh) validators

  mkOrdersExecutor @IO @IO @SerialT @TestEnv @C.BabbageEra backlogService syncSem transactions explorer poolResolver poolActions

mkTestExplorer :: forall f. (MonadIO f) => FullTxOut -> Map.Map TxOutRef FullTxOut -> Explorer f
mkTestExplorer executorOutput refMap = Explorer
  { getOutput                = \id   -> refTxOut2ExplorerOut `traverse` Map.lookup id refMap
  , getUnspentOutputsByPCred = \_ _  -> refTxOut2ExplorerOut executorOutput <&> (\out -> Items [out] 1)
  }

refTxOut2ExplorerOut :: forall f. (MonadIO f) => FullTxOut -> f Explorer.FullTxOut
refTxOut2ExplorerOut FullTxOut{fullTxOutRef=TxOutRef{..}, ..} = do
  validators <- mkValidators
  let
    ref = OutRef $ pack $ show txOutRefId ++ ":" ++ show txOutRefIdx
    txHash = TxHash (pack . show $ txOutRefId)
    test = serialiseAddress <$> toCardanoAddressInEra mockNetworkId fullTxOutAddress
    outAddr = Addr $ fromRight "incorrectAddress" test
    scriptsList = mapValidators (\ValidatorInfo{..} -> (address, scriptHash)) validators
    scriptHashMaybe = Map.lookup fullTxOutAddress (Map.fromList scriptsList)
    value = (\(cs, tn, qty) -> OutAsset (PolicyId (pack . show $ unCurrencySymbol cs)) (AssetName (pack . show $ unTokenName tn)) qty) <$> flattenValue fullTxOutValue
  pure Explorer.FullTxOut
    { ref           = ref
    , txHash        = txHash
    , index         = fromIntegral txOutRefIdx
    , globalIndex   = Gix 9999
    , addr          = outAddr
    , value         = [OutAsset (PolicyId "") (AssetName "") 1000000000]
    , dataHash      = Nothing
    , data'         = Nothing
    , spentByTxHash = Nothing
    , refScriptHash = scriptHashMaybe
    }

mkTestVault :: MonadThrow f => Vault f
mkTestVault =
  let
    keyPass = KeyPass "testKeyPass"
    tsStore = mkMockTrustStore
  in mkVault tsStore keyPass
