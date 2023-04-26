{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Gen.OrdersGen where

import Hedgehog 
  ( MonadGen )
import Hedgehog.Gen 
  ( bytes, int )
import qualified Hedgehog.Range as Range
import RIO
  ( (<&>), MonadIO )
import qualified Data.Map as Map

import PlutusTx.Builtins.Internal
  ( BuiltinByteString(..) )
import Plutus.V1.Ledger.Value
  ( Value, CurrencySymbol(CurrencySymbol), TokenName(TokenName), AssetClass(AssetClass), singleton )
import Plutus.V1.Ledger.Address 
  ( pubKeyHashAddress )
import Cardano.Api
  ( ScriptDatum(InlineScriptDatum) )
import CardanoTx.Value
  ( unionVal )
import Plutus.V2.Ledger.Api 
import qualified Plutus.Script.Utils.V2.Address as PV2
import qualified Cardano.Api          as C
import qualified Ledger.Tx.CardanoAPI as Interop
import qualified Ledger.Ada           as Ada

import qualified ErgoDex.Amm.Orders as Orders
import ErgoDex.Types
  ( ExFeePerToken(..), AssetAmount (AssetAmount), getAmount )
import CardanoTx.Models
  ( FullTxOut (FullTxOut)
  , TxOutDatum (KnownDatum, EmptyDatum)
  , fullTxOutRef
  , fullTxOutAddress
  , fullTxOutValue
  , fullTxOutDatum, fullTxOutScriptRef
  )
import ErgoDex.Contracts.Types
  ( coinAmountValue, Amount(Amount, unAmount), Coin(..) )
import ErgoDex.Amm.Orders
  ( AnyOrder (AnyOrder), OrderAction (SwapAction), Swap(..) )
import ErgoDex.Contracts.Pool 
  ( maxLqCap, PoolConfig(PoolConfig) )
import ErgoDex.State
  ( OnChain(..) )
import ErgoDex.Amm.Pool
  ( PoolId(..), Pool(..), outputAmount, PoolFee(..) ) 
import qualified ErgoDex.Contracts.Proxy.Swap as Contract

import qualified Spectrum.Executor.Types as Spectrum
import Spectrum.Executor.Config 
  ( TxRefs(..) )
import Gen.ConstantsGen 
  ( Validators, ValidatorInfo (ValidatorInfo, validator), mapValidators )

genBuiltinByteString :: MonadGen f => Int -> f BuiltinByteString
genBuiltinByteString s = bytes (Range.singleton s) <&> BuiltinByteString

genTxId :: MonadGen f => f TxId
genTxId = TxId <$> genBuiltinByteString 32

genPoolFee :: MonadGen f => f PoolFee
genPoolFee = pure $ PoolFee 995 1000
  
genPool :: MonadGen f => f Pool
genPool = do
  let range = Range.constant 1000 10000
  poolId         <- PoolId <$> genCoin
  poolCoinX      <- genCoin
  poolCoinY      <- genCoin
  poolCoinLq     <- genCoin
  poolReservesX  <- Amount . toInteger <$> int range
  poolReservesY  <- Amount . toInteger <$> int range
  let poolLiquidity = Amount (unAmount poolReservesX * unAmount poolReservesY)

  poolFee        <- genPoolFee
  let outCollateral  = 1000000000

  pure $ Pool poolId poolReservesX poolReservesY poolLiquidity poolCoinX poolCoinY poolCoinLq poolFee outCollateral

genTokenName :: (MonadGen f) => f TokenName
genTokenName = genBuiltinByteString 10 <&> TokenName

genCurrencySymbol :: (MonadGen f) => f CurrencySymbol
genCurrencySymbol = genBuiltinByteString 28 <&> CurrencySymbol

genCoin :: (MonadGen f) => f (Coin a)
genCoin = Coin <$> genAssetClass

genAssetClass :: (MonadGen f, Applicative f) => f AssetClass
genAssetClass = do
  tokenName <- genTokenName
  curSymbol <- genCurrencySymbol
  pure $ AssetClass (curSymbol, tokenName)

genFullTxOut :: (MonadGen f, Applicative f) => [Value] -> Address -> TxOutDatum -> TxId -> Integer -> f FullTxOut
genFullTxOut assets address datum txId txOutIndex =
    pure $ FullTxOut
    { fullTxOutRef     = TxOutRef txId txOutIndex
    , fullTxOutAddress = address
    , fullTxOutValue   = foldl unionVal mempty assets
    , fullTxOutDatum   = datum
    , fullTxOutScriptRef = Nothing
    }

genAdaFullTxOut :: (MonadGen f, Applicative f) => PubKeyHash -> f FullTxOut
genAdaFullTxOut pkh = do
    txId <- genTxId
    let adaValue = mkAdaValue 100000000000

    genFullTxOut [adaValue] (pubKeyHashAddress pkh) EmptyDatum txId 1

genFullTxOutForRefScript :: (MonadGen f, Applicative f) => Validator -> TxOutRef -> f FullTxOut
genFullTxOutForRefScript validator TxOutRef{..} =
    let
      address = PV2.mkValidatorAddress validator
      value = mkAdaValue 1000000000
    in genFullTxOut [value] address EmptyDatum txOutRefId txOutRefIdx

genOnChainPool :: (MonadGen f, Applicative f) => Validator -> f (OnChain Pool)
genOnChainPool poolValidator = do
  pool  <- genPool
  txOut <- genPoolTxOut poolValidator pool
  pure $ OnChain txOut pool

genPoolTxOut :: (MonadGen f, Applicative f) => Validator -> Pool -> f FullTxOut
genPoolTxOut pValidator Pool{..} = do
  let
    value   =
      [ mkAdaValue 1000000000
      , coinAmountValue (Coin (unCoin poolCoinX)) poolReservesX
      , coinAmountValue (Coin (unCoin poolCoinY)) poolReservesY
      , coinAmountValue (Coin (unCoin poolCoinLq)) (Amount (maxLqCap - unAmount poolLiquidity))
      , coinAmountValue (Coin (unCoin . unPoolId $ poolId)) 1
      ]
    address = PV2.mkValidatorAddress pValidator
    poolConfig = PoolConfig (unCoin . unPoolId $ poolId) (unCoin poolCoinX) (unCoin poolCoinY) (unCoin poolCoinLq) (poolFeeNum' poolFee)
    datum   = KnownDatum (Datum (toBuiltinData poolConfig))
    txOutIndex = 2
  txId <- genTxId
  genFullTxOut value address datum txId txOutIndex

genExFeePerToken :: MonadGen f => f ExFeePerToken
genExFeePerToken = do
  let range = Range.constant 100 10000000
  exFeePerTokenNum <- toInteger <$> int range
  exFeePerTokenDen <- toInteger <$> int range
  pure $ ExFeePerToken exFeePerTokenNum exFeePerTokenDen

genExFeePerTokenFromQuoteAmount :: MonadGen f => Integer -> f ExFeePerToken
genExFeePerTokenFromQuoteAmount quoteAmount = do
  let
    range = Range.constant 100 10000000
    minAdaOutput = 2000000
    den = 10000000000000000
    num = minAdaOutput * den `div` quoteAmount
  exFeePerTokenDen <- toInteger <$> int range
  pure $ ExFeePerToken num den

mkAdaAssetClass :: AssetClass
mkAdaAssetClass = AssetClass (Ada.adaSymbol, Ada.adaToken)

mkValue :: AssetClass -> Integer -> Value
mkValue (AssetClass (cs, tn)) = singleton cs tn

mkAdaValue :: Integer -> Value
mkAdaValue = mkValue mkAdaAssetClass

genPubKeyHash :: MonadGen f => f PubKeyHash
genPubKeyHash = genBuiltinByteString 28 <&> PubKeyHash

genSwapOrder :: forall f. (MonadGen f, Applicative f) => Validator -> Pool -> f Spectrum.Order
genSwapOrder swapInstance pool@Pool{..} = do
  pkh <- genPubKeyHash
  let
    baseAmountRange  = Range.constant 500 (fromInteger . unAmount $ poolReservesX - 100)

  baseAmount    <- Amount . toInteger <$> int baseAmountRange
  let
    swapXAmount = AssetAmount (Coin (unCoin poolCoinX)) baseAmount
    quoteAmount = getAmount $ outputAmount pool swapXAmount
  
  exFeePerToken <- genExFeePerTokenFromQuoteAmount (unAmount quoteAmount)
  let
    swapOrder  = Orders.Swap
      { swapPoolId      = poolId
      , swapBaseIn      = baseAmount
      , swapMinQuoteOut = quoteAmount
      , swapBase        = Coin (unCoin poolCoinX)
      , swapQuote       = Coin (unCoin poolCoinY)
      , swapExFee       = exFeePerToken
      , swapRewardPkh   = pkh
      , swapRewardSPkh  = Nothing
      }
    swapCfg = Contract.SwapConfig
      { base             = unCoin poolCoinX
      , quote            = unCoin poolCoinY
      , poolNft          = unCoin . unPoolId $ poolId
      , feeNum           = poolFeeNum' poolFee
      , exFeePerTokenNum = exFeePerTokenNum exFeePerToken
      , exFeePerTokenDen = exFeePerTokenDen exFeePerToken
      , rewardPkh        = pkh
      , stakePkh         = Nothing
      , baseAmount       = unAmount baseAmount
      , minQuoteAmount   = unAmount quoteAmount
     }
    values       = foldl unionVal (mkAdaValue 1000000000) [coinAmountValue (Coin (unCoin poolCoinX)) baseAmount]
    swapAddr     = PV2.mkValidatorAddress swapInstance
    swapAction   = SwapAction swapOrder
    swapAnyOrder = AnyOrder poolId swapAction

  txId  <- genTxId
  txOut <- genFullTxOut [values] swapAddr (KnownDatum (Datum . toBuiltinData $ swapCfg)) txId 1
  pure $ OnChain txOut swapAnyOrder

genTxOutRef :: (MonadGen f) => f TxId
genTxOutRef = genBuiltinByteString 32 <&> TxId

genRefMapForExplorer :: forall f. (MonadGen f, Applicative f) => Validators -> TxRefs -> f (Map.Map TxOutRef FullTxOut)
genRefMapForExplorer validatorInfo TxRefs{..} = do
  let
    validatorList = mapValidators (\ValidatorInfo{..} -> validator) validatorInfo
    refsList = [swapRef, depositRef, redeemRef, poolRef] `zip` validatorList
  mainList <- packRefAndValidator `traverse` refsList
  pure $ Map.fromList mainList
  where
    packRefAndValidator :: (MonadGen f, Applicative f) => (C.TxIn, Validator) -> f (TxOutRef, FullTxOut)
    packRefAndValidator (txIn, validator) = do
      let
        ref = Interop.fromCardanoTxIn txIn
      txOut <- genFullTxOutForRefScript validator ref
      pure (ref, txOut)
