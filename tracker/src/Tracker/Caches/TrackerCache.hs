module Tracker.Caches.TrackerCache where

import Explorer.Types

import Tracker.Models.AppConfig

import GHC.Natural
import Prelude
import Database.Redis               as Redis
import Debug.Trace                  as Debug
import Data.ByteString.UTF8         as BSU 
import Data.ByteString              as BS
import Control.Monad.Trans.Resource
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import RIO

data TrackerCache f = TrackerCache
  { putMinIndex :: Gix -> f ()
  , getMinIndex :: f Gix
  }

mkTrackerCache
  :: (MonadIO f) 
  => RedisSettings
  -> TrackerProgrammConfig
  -> ResourceT f (TrackerCache f)
mkTrackerCache settings tConfig =
    fmap (\c -> TrackerCache (putMinIndex' c) (getMinIndex' c tConfig)) poolR
  where
    poolR = mkConnectionPool settings

mkConnectionPool
  :: (MonadIO f) 
  => RedisSettings
  -> ResourceT f Connection
mkConnectionPool RedisSettings{..} =
  lift $ liftIO $ checkedConnect
    defaultConnectInfo 
      { connectHost = redisHost
      }

putMinIndex'
  :: (MonadIO f)
  => Connection
  -> Gix
  -> f ()
putMinIndex' conn newMinIndex =
  void $ liftIO $ runRedis conn $ do
    Redis.set "min_index" (BSU.fromString $ show newMinIndex)

getMinIndex'
  :: (MonadIO f)
  => Connection
  -> TrackerProgrammConfig
  -> f Gix
getMinIndex' conn TrackerProgrammConfig{..} = liftIO $ do
  res <- runRedis conn $ Redis.get "min_index"
  pure $ getCorrectIndex res (Gix . naturalToInteger $ minIndex)

-- todo log err
getCorrectIndex :: forall c. Show c => Either c (Maybe BS.ByteString) -> Gix -> Gix
getCorrectIndex input defaultInput@(Gix defaultInputValue) =
  case input of
    Left err    -> defaultInput
    Right value ->
      case value of
        Just v ->
          let
            str = BSU.toString v
            minIndexInCache = read str :: Integer
          in
            if (minIndexInCache < defaultInputValue) then defaultInput else (Gix minIndexInCache)
        _ -> defaultInput