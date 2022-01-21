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
      --todo: only for debug
      poolCred    = PaymentCred "addr_test1wpl3evs3lr2z8dlmtr247090lpp5v7sy6kkkr5wgkvzhw0gneur3h"
      depositCred = PaymentCred "addr_test1wp6lpvw9wayyc4dpzp3g0fj2j2mn8rtyt3gtlcttqu3wkcg6crfnh"
      paging = Paging 0 10
  _        <- Log.log $ "Min index is " ++ show minIndex
  -- todo: only for debug
  poolOutputs    <- getOutputsWithRetry explorer poolCred paging
  depositOutputs <- getOutputsWithRetry explorer depositCred paging
  let common = Items (items poolOutputs ++ items depositOutputs) (total poolOutputs + total depositOutputs)
  _        <- Log.log $ "Min index is " ++ show minIndex
  _        <- Log.log $ "value:" ++ show (total common)
  _        <- putMinIndex $ Gix $ unGix minIndex + toInteger (length $ items common)
  pure $ items common

getOutputsWithRetry
  :: (Monad f, MonadIO f, MonadMask f)
  => Explorer f
  -> PaymentCred
  -> Paging
  -> f (Items FullTxOut)
getOutputsWithRetry Explorer{..} cred paging = getUnspentOutputsByPCred cred paging

--  recoverAll (constantDelay 10000000) toRet
--    where
--      toRet status = Log.log ("Going to get data from explorer. Current retry status: " ++ (show status)) >> getUnspentOutputsByPCred cred paging