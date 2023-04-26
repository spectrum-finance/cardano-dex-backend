{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Gen.ConstantsGen where

import Hedgehog
  ( MonadGen )
import GHC.Generics
  ( Generic )
import RIO
  ( MonadReader, MonadIO )
import Data.Ratio 
  ( (%) )
import Data.Either.Extra
  ( maybeToEither )
import RIO.Time
  ( NominalDiffTime )
import RIO.Prelude
  ( either, ask, local, (<&>), fromMaybe, fromRight, first )
import qualified Data.ByteString.Base16 as Base16 
import qualified Data.Map as Map

import Cardano.Api.Shelley 
  ( ProtocolParameters(..) )
import Cardano.Api
import Cardano.Ledger.Shelley.API 
  ( Coin(Coin) )
import Cardano.Ledger.Babbage.Translation 
  ( coinsPerUTxOWordToCoinsPerUTxOByte )
import PlutusCore 
  ( defaultCostModelParams )
import Ledger 
  ( unValidatorScript, Address )
import Plutus.Script.Utils.V2.Address 
  ( mkValidatorAddress )
import CardanoTx.Address
  ( readShellyAddress )
import qualified Cardano.Api as C
import qualified Plutus.Script.Utils.V2.Scripts as P
import qualified Plutus.V2.Ledger.Api as P
import qualified Data.ByteString as BS

import Spectrum.Executor.Config
  ( TxRefs(..) )
import System.Logging.Hlog
  ( MakeLogging )
import Gen.LoggingGen
  ( mkMakeLogging, mkEmptyMakeLogging )
import ErgoDex.PValidators
  ( poolValidator, redeemValidator, depositValidator, swapValidator )
import Spectrum.Executor.Backlog.Config
  ( BacklogServiceConfig(..) )
import CardanoTx.Models

cfgForOnlyPendingOrders :: BacklogServiceConfig
cfgForOnlyPendingOrders = BacklogServiceConfig
  { orderLifetime        = 900 :: NominalDiffTime
  , orderExecTime        = 600 :: NominalDiffTime
  , suspendedPropability = 0
  }

data ValidatorInfo = ValidatorInfo
  { address    :: Ledger.Address
  , scriptHash :: P.ScriptHash
  , validator  :: P.Validator
  }

data Validators = Validators
  { swapInfo    :: ValidatorInfo
  , depositInfo :: ValidatorInfo
  , redeemInfo  :: ValidatorInfo
  , poolInfo    :: ValidatorInfo
  }

mkValidators :: MonadIO f => f Validators
mkValidators = do
  swapInfo    <- mkValidatorInfo <$> swapValidator
  depositInfo <- mkValidatorInfo <$> depositValidator
  redeemInfo  <- mkValidatorInfo <$> redeemValidator
  poolInfo    <- mkValidatorInfo <$> poolValidator
  pure Validators{..}
  where
    mkValidatorInfo :: P.Validator -> ValidatorInfo
    mkValidatorInfo scriptValidator = ValidatorInfo
      { address = mkValidatorAddress scriptValidator
      , scriptHash = P.scriptHash . unValidatorScript $ scriptValidator
      , validator = scriptValidator
      }

mapValidators :: (ValidatorInfo -> a) -> Validators -> [a]
mapValidators func Validators{..} =
  [swapInfo, depositInfo, redeemInfo, poolInfo] <&> func

traverseValidators :: Applicative f => (ValidatorInfo -> f a) -> Validators -> f [a]
traverseValidators funcF Validators{..} =
  funcF `traverse` [swapInfo, depositInfo, redeemInfo, poolInfo]

staticProtocolParams :: ProtocolParameters
staticProtocolParams = ProtocolParameters
  { protocolParamProtocolVersion = (6,0)
    , protocolParamDecentralization = Just (3 % 5)
    , protocolParamExtraPraosEntropy = Nothing
    , protocolParamMaxBlockHeaderSize = 1100
    , protocolParamMaxBlockBodySize = 65536
    , protocolParamMaxTxSize = 16384
    , protocolParamTxFeeFixed = 155381
    , protocolParamTxFeePerByte = 44
    , protocolParamMinUTxOValue = Nothing
    , protocolParamStakeAddressDeposit = Lovelace 2000000
    , protocolParamStakePoolDeposit = Lovelace 500000000
    , protocolParamMinPoolCost = Lovelace 340000000
    , protocolParamPoolRetireMaxEpoch = EpochNo 18
    , protocolParamStakePoolTargetNum = 150
    , protocolParamPoolPledgeInfluence = 3 % 10
    , protocolParamMonetaryExpansion = 3 % 1000
    , protocolParamTreasuryCut = 1 % 5
    , protocolParamUTxOCostPerWord = Nothing -- Obsolete from babbage onwards
    , protocolParamCostModels = Map.fromList
      [ (AnyPlutusScriptVersion PlutusScriptV1, CostModel $ fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams)
      , (AnyPlutusScriptVersion PlutusScriptV2, CostModel $ fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams) ]
    , protocolParamPrices = Just (ExecutionUnitPrices {priceExecutionSteps = 721 % 10000000, priceExecutionMemory = 577 % 10000})
    , protocolParamMaxTxExUnits = Just (ExecutionUnits {executionSteps = 10000000000, executionMemory = 10000000})
    , protocolParamMaxBlockExUnits = Just (ExecutionUnits {executionSteps = 40000000000, executionMemory = 50000000})
    , protocolParamMaxValueSize = Just 5000
    , protocolParamCollateralPercent = Just 150
    , protocolParamMaxCollateralInputs = Just 3
    , protocolParamUTxOCostPerByte =
        let (Coin coinsPerUTxOByte) = coinsPerUTxOWordToCoinsPerUTxOByte $ Coin 34482
         in Just $ Lovelace coinsPerUTxOByte
    }

testAddress :: Ledger.Address
testAddress = fromRight (error "AddrCreationError") $ maybeToEither "Addr reading error" (readShellyAddress "addr_test1vr007v5nktnksje3gnm4aw4arwrkcl5rvvx4lwa3w8mtzxgf6c2nt")

testTxRefs :: TxRefs
testTxRefs = TxRefs
  { swapRef    = C.TxIn (C.TxId "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16") (C.TxIx 1)
  , depositRef = C.TxIn (C.TxId "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16") (C.TxIx 2)
  , redeemRef  = C.TxIn (C.TxId "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16") (C.TxIx 3)
  , poolRef    = C.TxIn (C.TxId "b2f79375bf73234bb988cfdb911c78ac4e9b5470197e828d507babfdcca08d16") (C.TxIx 4)
  }

data TestEnv = TestEnv {
  logging :: MakeLogging IO IO,
  txRefs  :: TxRefs
} deriving (Generic)

mockTestEnv :: TestEnv
mockTestEnv = TestEnv
  { logging = mkEmptyMakeLogging
  , txRefs  = testTxRefs
  }

instance MonadReader TestEnv IO where
  ask = pure mockTestEnv
  local = \_ a -> a

mockNetworkId :: NetworkId
mockNetworkId = C.Testnet (C.NetworkMagic 12345)

signingKey :: C.SigningKey C.PaymentKey
signingKey = fromRight (error "signingKey: parseError") $ parseSigningKeyBase16 "582087a8720e8769c2ba34d4cf17f95b8ec286a89becd3bb025ee6aeb2cf4dbd588b"

parseSigningKeyBase16 :: BS.ByteString -> Either String (C.SigningKey C.PaymentKey)
parseSigningKeyBase16 k
  = RIO.Prelude.either
    (const $ Left $ "parseSigningKeyBase16: ill-formed base16 encoding")
    (parseSigningKeyTE . asTE)
    (Base16.decode k)
  where
    asTE addr = C.TextEnvelope {
          teType = "PaymentSigningKeyShelley_ed25519"
        , teDescription = "Payment Signing Key"
        , teRawCBOR = addr
        }

parseSigningKeyTE :: C.TextEnvelope -> Either String (C.SigningKey C.PaymentKey)
parseSigningKeyTE
  = first (const "parseSigningKeyTE error") . C.deserialiseFromTextEnvelopeAnyOf acceptedTypes

acceptedTypes :: [C.FromSomeType C.HasTextEnvelope (C.SigningKey C.PaymentKey)]
acceptedTypes =
  [ C.FromSomeType (C.AsSigningKey C.AsPaymentKey) id
  ]
