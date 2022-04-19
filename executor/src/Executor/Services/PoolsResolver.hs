{-# LANGUAGE OverloadedStrings #-}

module Executor.Services.PoolsResolver
  ( PoolsResolver(..)
  , mkPoolsResolver
  ) where

import Executor.Models.Config
import Debug.Trace
          
import ErgoDex.Amm.Pool
import System.Logging.Hlog
import Core.Types

import RIO
import Network.HTTP.Simple
import Data.ByteString.Char8
import GHC.Natural           

data PoolsResolver f = PoolsResolver
  { resolvePool   :: PoolId        -> f (Maybe ConfirmedPool)
  , sendPredicted :: PredictedPool -> f ()
  }

mkPoolsResolver 
  :: (Monad i, MonadIO f)
  => PoolsResolverConfig
  -> MakeLogging i f
  -> i (PoolsResolver f)
mkPoolsResolver settings MakeLogging{..} = do
  logger <- forComponent "poolsResolver"
  pure $ PoolsResolver (resolvePool' settings logger) (sendPredicted' settings)

resolvePool' 
  :: (MonadIO f)
  => PoolsResolverConfig
  -> Logging f
  -> PoolId 
  -> f (Maybe ConfirmedPool)
resolvePool' PoolsResolverConfig{..} Logging{..} poolId = do
  _ <- infoM ("Going to resolve pool with id:" ++ (show poolId))
  let
    request = defaultRequest
      & setRequestPath (pack "resolve") 
      & setRequestHost (pack getHost) 
      & setRequestPort (naturalToInt getPort)
      & setRequestMethod (pack "POST")
      & setRequestBodyJSON poolId

  response <- httpJSON request
  pure $ getResponseBody response

sendPredicted'
  :: (MonadIO f)
  => PoolsResolverConfig 
  -> PredictedPool 
  -> f ()
sendPredicted' PoolsResolverConfig{..} predicted = do
  let 
    request = defaultRequest
      & setRequestPath (pack "update") 
      & setRequestHost (pack getHost) 
      & setRequestPort (naturalToInt getPort)
      & setRequestMethod (pack "POST")
      & setRequestBodyJSON predicted

  void $ httpNoBody request