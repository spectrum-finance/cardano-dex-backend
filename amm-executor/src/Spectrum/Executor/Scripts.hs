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

data ScriptsValidators = ScriptsValidators
  { swapValidator    :: PV2.Validator 
  , depositValidator :: PV2.Validator
  , redeemValidator  :: PV2.Validator
  , poolValidator    :: PV2.Validator
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
  pure $ ScriptsValidators{..}
      
readValidatorFromFile :: (MonadIO m) => FilePath -> m PV2.Validator
readValidatorFromFile path = do
  bytes <- liftIO $ BSL.readFile path
  pure $ deserialise bytes