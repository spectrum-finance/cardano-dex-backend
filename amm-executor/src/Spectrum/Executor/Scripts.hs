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
  ( Version(..), PoolValidator (..), OrderValidator (..) )
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)

data ScriptsValidators = ScriptsValidators
  { swapValidator    :: PV2.Validator
  , swapAddress      :: PV2.Address
  , depositValidator :: PV2.Validator
  , depositAddress   :: PV2.Address
  , redeemValidator  :: PV2.Validator
  , redeemAddress    :: PV2.Address
  , poolV1Validator  :: PV2.Validator
  , poolV1Address    :: PV2.Address
  , poolV2Validator  :: PV2.Validator
  , poolV2Address    :: PV2.Address
  }

scriptsValidators2AmmValidatorsV1 :: ScriptsValidators -> AmmValidators 'V1
scriptsValidators2AmmValidatorsV1 ScriptsValidators{..} =
  let
    poolV = PoolValidator poolV1Validator
    swapV = SwapValidator swapValidator
    redeemV = RedeemValidator redeemValidator
    depositV = DepositValidator depositValidator
  in AmmValidators{..}

scriptsValidators2AmmValidatorsV2 :: ScriptsValidators -> AmmValidators 'V2
scriptsValidators2AmmValidatorsV2 ScriptsValidators{..} =
  let
    poolV = PoolValidator poolV2Validator
    swapV = SwapValidator swapValidator
    redeemV = RedeemValidator redeemValidator
    depositV = DepositValidator depositValidator
  in AmmValidators{..}

mkScriptsValidators :: (MonadIO m) => ScriptsConfig -> m ScriptsValidators
mkScriptsValidators ScriptsConfig{..} = do
  swapValidator    <- readValidatorFromFile swapScriptPath
  redeemValidator  <- readValidatorFromFile redeemScriptPath
  depositValidator <- readValidatorFromFile depositScriptPath
  poolV1Validator  <- readValidatorFromFile poolV1ScriptPath
  poolV2Validator  <- readValidatorFromFile poolV2ScriptPath
  let
    swapAddress    = mkValidatorAddress swapValidator
    depositAddress = mkValidatorAddress depositValidator
    redeemAddress  = mkValidatorAddress redeemValidator
    poolV1Address  = mkValidatorAddress poolV1Validator
    poolV2Address  = mkValidatorAddress poolV2Validator
  pure $ ScriptsValidators{..}
      
readValidatorFromFile :: (MonadIO m) => FilePath -> m PV2.Validator
readValidatorFromFile path = do
  bytes <- liftIO $ BSL.readFile path
  pure $ deserialise bytes