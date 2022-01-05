module Tracker.Services.TrackerService
  ( TrackerService(..)
  , mkTrackerService
  ) where

import Tracker.Models.AppConfig
import Tracker.Caches.TrackerCache
import Tracker.Services.Logger as Log

import Explorer.Models
import Explorer.Service
import Explorer.Types

import Prelude
import GHC.Natural as Natural ( naturalToInt )
import RIO

data TrackerService f = TrackerService
 { getOutputs :: f [FullTxOut]
 }

mkTrackerService
  :: (Monad f, MonadIO f)
  => TrackerServiceConfig
  -> TrackerCache f
  -> Explorer f
  -> TrackerService f
mkTrackerService settings cache client = TrackerService $ getOutputs' settings cache client

getOutputs'
  :: (Monad f, MonadIO f)
  => TrackerServiceConfig
  -> TrackerCache f
  -> Explorer f
  -> f [FullTxOut]
getOutputs' TrackerServiceConfig{..} TrackerCache{..} Explorer{..} = do
  _        <- Log.log "Going to fetch min index"
  testIndex@(Gix res) <- getMinIndex
  let minIndex = if (res < 8092461) then Gix 8092461 else testIndex
      cred = PaymentCred "addr_test1wr22y9ksk3ar5fcx7d0ac8laktzy0zegx96zt2cg3ehsrccyp6we9"
      paging = Paging 0 10
  _        <- Log.log $ "Min index is " ++ show minIndex
  outputs  <- getUnspentOutputsByPCred cred paging
  _        <- Log.log $ "Min index is " ++ show minIndex
  _        <- putMinIndex $ Gix $ unGix minIndex + toInteger (length $ items outputs)
  pure $ items outputs