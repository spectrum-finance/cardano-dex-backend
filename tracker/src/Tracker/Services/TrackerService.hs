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
      actionCred    = PaymentCred "addr_test1wqhzjndj0j8wky2jastjugg6tadnnj4nqfvprzfwttt2hks8rc0mk"
      poolCred = PaymentCred "addr_test1wrtv0h7pwymjuy7d74ec0x6akthw7vz77mjav5yzvuxuysg23aw9a"
      paging = Paging 0 10
  _        <- Log.log $ "Min index is " ++ show minIndex
  -- todo: only for debug
  poolOutputs    <- getOutputsWithRetry explorer poolCred paging
  depositOutputs <- getOutputsWithRetry explorer actionCred paging
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