{-# LANGUAGE OverloadedStrings #-}

module Executor.Services.PoolsResolver
  ( PoolsResolver(..)
  , mkPoolsResolver
  ) where

import Executor.Models.Config
import Debug.Trace
          
import ErgoDex.Amm.Pool
import Executor.Clients.PoolsResolverClient
import System.Logging.Hlog
import Core.Types

import RIO
import Network.HTTP.Simple
import Data.ByteString.Char8
import Control.Monad.Catch
import GHC.Natural           
import Control.Retry

data PoolsResolver f = PoolsResolver
  { resolvePool   :: PoolId        -> f (Maybe ConfirmedPool)
  , sendPredicted :: PredictedPool -> f ()
  }

mkPoolsResolver 
  :: (Monad i, MonadIO f, MonadMask f)
  => PoolsResolverConfig
  -> PoolsResolverClient f
  -> MakeLogging i f
  -> i (PoolsResolver f)
mkPoolsResolver settings client MakeLogging{..} = do
  logger <- forComponent "poolsResolver"
  pure $ PoolsResolver (resolvePool' settings client logger) (sendPredicted' settings client logger)

resolvePool' 
  :: (MonadIO f, MonadMask f)
  => PoolsResolverConfig
  -> PoolsResolverClient f
  -> Logging f
  -> PoolId 
  -> f (Maybe ConfirmedPool)
resolvePool' PoolsResolverConfig{..} PoolsResolverClient{..} Logging{..} poolId = do
  _ <- infoM ("Going to resolve pool with id:" ++ (show poolId))
  let limitedBackoff = constantDelay 1000000 <> limitRetries (naturalToInt maxAttempts)
  recoverAll limitedBackoff (\rs -> infoM ("RetryStatus for resolvePool " ++ (show rs)) >> resolvePool poolId)

sendPredicted'
  :: (MonadIO f, MonadMask f)
  => PoolsResolverConfig
  -> PoolsResolverClient f
  -> Logging f
  -> PredictedPool
  -> f ()
sendPredicted' PoolsResolverConfig{..} PoolsResolverClient{..} Logging{..} predicted = do
  let limitedBackoff = constantDelay 1000000 <> limitRetries (naturalToInt maxAttempts)
  recoverAll limitedBackoff (\rs -> infoM ("RetryStatus for sendPredicted " ++ (show rs)) >> (sendPredicted predicted))