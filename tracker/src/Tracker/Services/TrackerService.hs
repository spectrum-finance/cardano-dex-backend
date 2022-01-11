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
import Control.Monad.Catch
import Control.Retry
import RIO

data TrackerService f = TrackerService
 { getOutputs :: f [FullTxOut]
 }

mkTrackerService
  :: (Monad f, MonadIO f, MonadMask f)
  => TrackerServiceConfig
  -> TrackerCache f
  -> Explorer f
  -> TrackerService f
mkTrackerService settings cache client = TrackerService $ getOutputs' settings cache client

getOutputs'
  :: (Monad f, MonadIO f, MonadMask f)
  => TrackerServiceConfig
  -> TrackerCache f
  -> Explorer f
  -> f [FullTxOut]
getOutputs' TrackerServiceConfig{..} TrackerCache{..} explorer = do
  _        <- Log.log "Going to fetch min index"
  testIndex@(Gix res) <- getMinIndex
  let minIndex = if (res < 8092461) then Gix 8092461 else testIndex
      cred = PaymentCred "addr_test1wr7tmuxs8lkql7x3pj70zzsx9a9svgdm669z4j9kq79trngs5cyyt"
      paging = Paging 0 10
  _        <- Log.log $ "Min index is " ++ show minIndex
  outputs  <- getOutputsWithRetry explorer cred paging
  _        <- Log.log $ "Min index is " ++ show minIndex
  _        <- putMinIndex $ Gix $ unGix minIndex + toInteger (length $ items outputs)
  pure $ items outputs

getOutputsWithRetry
  :: (Monad f, MonadIO f, MonadMask f)
  => Explorer f
  -> PaymentCred
  -> Paging
  -> f (Items FullTxOut)
getOutputsWithRetry Explorer{..} cred paging =
  recoverAll (constantDelay 10000000) toRet
    where
      toRet _ = getUnspentOutputsByPCred cred paging