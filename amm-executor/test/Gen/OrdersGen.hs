{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Gen.OrdersGen where

import Plutus.V1.Ledger.Value
import qualified Data.Text as T
import Hedgehog (MonadGen)
import RIO
import Hedgehog.Range as Range
import qualified PlutusTx
import qualified Ledger.Ada      as Ada
import PlutusTx.Builtins.Internal
import Hedgehog.Gen (bytes, int)
import Ledger
    ( TxId(TxId), TxOutRef(TxOutRef), Address, PubKeyHash (PubKeyHash), Datum (Datum) )
import Plutus.V1.Ledger.Value    as Value
import ErgoDex.Types
import qualified Spectrum.Executor.Types as Spectrum
import CardanoTx.Models ( FullTxOut (FullTxOut), TxOutDatum (KnownDatum), fullTxOutRef, fullTxOutAddress, fullTxOutValue, fullTxOutDatum )
import CardanoTx.Value (unionVal)
import ErgoDex.Contracts.Types
import ErgoDex.Amm.Orders (AnyOrder (AnyOrder), OrderAction (SwapAction), Swap(..))
import qualified ErgoDex.Amm.Orders as Orders
import ErgoDex.Contracts.Pool
import ErgoDex.Amm.Pool (PoolId (PoolId), Pool (Pool), PoolFee (PoolFee), poolReservesY, poolReservesX, poolId, poolCoinX, poolCoinY, unPoolId)
import Ledger.Typed.Scripts.Validators
import ErgoDex.Contracts.Proxy.OffChain (swapInstance)
import Plutus.V1.Ledger.Api (toBuiltin, ToData (toBuiltinData))
import qualified ErgoDex.Contracts.Proxy.Swap as Contract
import qualified ErgoDex.Amm.Orders as Contract
import ErgoDex.State (OnChain(OnChain))

genBuiltinByteString :: MonadGen f => Int -> f BuiltinByteString
genBuiltinByteString s = bytes (Range.singleton s) <&> BuiltinByteString

genTxId :: MonadGen f => f TxId
genTxId = TxId <$> genBuiltinByteString 32

genPoolFee :: MonadGen f => f PoolFee
genPoolFee = do
  let range = constant 100 1000
  poolFeeNum' <- toInteger <$> int range
  poolFeeDen' <- toInteger <$> int range
  pure $ PoolFee poolFeeNum' poolFeeDen'

genPool :: MonadGen f => f Pool
genPool = do
  let range = constant 100 10000000
  poolId      <- PoolId <$> genCoin
  poolCoinX   <- genCoin
  poolCoinY   <- genCoin
  poolCoinLq  <- genCoin
  poolReservesX  <- Amount . toInteger <$> int range
  poolReservesY  <- Amount . toInteger <$> int range
  let poolLiquidity = Amount (unAmount poolReservesX * unAmount poolReservesY)
  poolFee <- genPoolFee
  outCollateral  <- Amount . toInteger <$> int range
  pure $ Pool poolId poolReservesX poolReservesY poolLiquidity poolCoinX poolCoinY poolCoinLq  poolFee outCollateral

genTokenName :: (MonadGen f) => f TokenName
genTokenName = genBuiltinByteString 32 <&> TokenName

genCurrencySymbol :: (MonadGen f) => f CurrencySymbol
genCurrencySymbol = genBuiltinByteString 32 <&> CurrencySymbol

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
    { fullTxOutRef = TxOutRef txId txOutIndex
    , fullTxOutAddress = address
    , fullTxOutValue = foldl unionVal mempty assets
    , fullTxOutDatum = datum
    }

genExFeePerToken :: MonadGen f => f ExFeePerToken
genExFeePerToken = do
  let range = constant 100 10000000
  exFeePerTokenNum <- toInteger <$> int range
  exFeePerTokenDen <- toInteger <$> int range
  pure $ ExFeePerToken exFeePerTokenNum exFeePerTokenDen

mkAdaAssetClass :: AssetClass
mkAdaAssetClass = AssetClass (Ada.adaSymbol, Ada.adaToken)

mkValue :: AssetClass -> Integer -> Value
mkValue (AssetClass (cs, tn)) = Value.singleton cs tn

mkAdaValue :: Integer -> Value
mkAdaValue = mkValue mkAdaAssetClass

genPubKeyHash :: MonadGen f => f PubKeyHash
genPubKeyHash = genBuiltinByteString 32 <&> PubKeyHash

genSwapOrder :: forall f. (MonadGen f, Applicative f) => PubKeyHash -> Pool -> f Spectrum.Order
genSwapOrder pkh Pool{..} = do
  let
    baseAmountRange  = constant 1 (fromInteger . unAmount $ poolReservesX)
    quoteAmountRange = constant 1 (fromInteger . unAmount $ poolReservesY)
  baseAmount    <- Amount . toInteger <$> int baseAmountRange
  quoteAmount   <- Amount . toInteger <$> int quoteAmountRange
  exFeePerToken <- genExFeePerToken
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
      , quote            = unCoin poolCoinX
      , poolNft          = unCoin . unPoolId $ poolId
      , feeNum           = 995
      , exFeePerTokenNum = exFeePerTokenNum exFeePerToken
      , exFeePerTokenDen = exFeePerTokenDen exFeePerToken
      , rewardPkh        = pkh
      , stakePkh         = Nothing
      , baseAmount       = unAmount baseAmount
      , minQuoteAmount   = unAmount quoteAmount
     }
    values = foldl unionVal (mkAdaValue 1000000) [coinAmountValue (Coin (unCoin poolCoinX)) baseAmount, coinAmountValue (Coin (unCoin poolCoinY)) quoteAmount]
    swapAddr = validatorAddress swapInstance
    swapAction = SwapAction swapOrder
    swapAnyOrder = AnyOrder poolId swapAction
  txId  <- genTxId
  txOut <- genFullTxOut [values] swapAddr (KnownDatum (Datum . toBuiltinData $ swapCfg)) txId 1
  pure $ OnChain txOut swapAnyOrder

genTxOutRef :: (MonadGen f) => f TxId
genTxOutRef = genBuiltinByteString 32 <&> TxId