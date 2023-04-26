module Spectrum.Executor.Scripts where

import Codec.Serialise 
  ( deserialise )
import RIO
  ( MonadIO (liftIO) )

import qualified Data.ByteString.Lazy as BSL

import qualified Plutus.V2.Ledger.Api as PV2

import Spectrum.Executor.Config 
  ( ScriptsConfig(..) )
import ErgoDex.Amm.PoolActions 
  ( AmmValidators (..) )
import ErgoDex.Validators 
  ( V1, PoolValidator (..), OrderValidator (..) )
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)

data ScriptsValidators = ScriptsValidators
  { swapValidator    :: PV2.Validator
  , swapAddress      :: PV2.Address
  , depositValidator :: PV2.Validator
  , depositAddress   :: PV2.Address
  , redeemValidator  :: PV2.Validator
  , redeemAddress    :: PV2.Address
  , poolValidator    :: PV2.Validator
  , poolAddress      :: PV2.Address
  }

scriptsValidators2AmmValidators :: ScriptsValidators -> AmmValidators V1
scriptsValidators2AmmValidators ScriptsValidators{..} =
  let
    poolV = PoolValidator poolValidator
    swapV = SwapValidator swapValidator
    redeemV = RedeemValidator redeemValidator
    depositV = DepositValidator depositValidator
  in AmmValidators{..}

mkScriptsValidators :: (MonadIO m) => ScriptsConfig -> m ScriptsValidators
mkScriptsValidators ScriptsConfig{..} = do
  swapValidator    <- readValidatorFromFile swapScriptPath
  redeemValidator  <- readValidatorFromFile redeemScriptPath
  depositValidator <- readValidatorFromFile depositScriptPath
  poolValidator    <- readValidatorFromFile poolScriptPath
  let
    swapAddress    = mkValidatorAddress swapValidator
    depositAddress = mkValidatorAddress depositValidator
    redeemAddress  = mkValidatorAddress redeemValidator
    poolAddress    = mkValidatorAddress poolValidator
  pure $ ScriptsValidators{..}
      
readValidatorFromFile :: (MonadIO m) => FilePath -> m PV2.Validator
readValidatorFromFile path = do
  bytes <- liftIO $ BSL.readFile path
  pure $ deserialise bytes