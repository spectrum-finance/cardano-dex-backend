module Executor.Services.PoolsResolver
  ( PoolsResolver(..)
  , mkPoolsResolver
  ) where

import Executor.Models.Config 
          
import ErgoDex.State
import ErgoDex.Amm.Pool
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
  :: MonadIO f 
  => PoolsResolverConfig 
  -> PoolsResolver f
mkPoolsResolver settings = 
  PoolsResolver (resolvePool' settings) (sendPredicted' settings)

resolvePool' 
  :: (MonadIO f)
  => PoolsResolverConfig 
  -> PoolId 
  -> f (Maybe ConfirmedPool)
resolvePool' PoolsResolverConfig{..} poolId = do
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