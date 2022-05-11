module Tracker.Caches.TrackerCache 
  ( TrackerCache(..)
  , mkTrackerCache
  ) where

import Explorer.Types

import Tracker.Models.AppConfig

import GHC.Natural
import Prelude
import System.Logging.Hlog          (Logging(Logging, debugM), MakeLogging(..))
import Data.ByteString.UTF8         as BSU
import Data.ByteString              as BS
import Control.Monad.Trans.Resource
import Control.Monad.IO.Unlift
import RIO

import qualified Database.RocksDB   as Rocks

data TrackerCache f = TrackerCache
  { putMinIndex :: Gix -> f ()
  , getMinIndex :: f Gix
  }

minIndexKey :: ByteString 
minIndexKey = "min_index"

mkTrackerCache
  :: (MonadIO i, MonadResource i, MonadIO f)
  => TrackerStoreSettings
  -> TrackerProgrammConfig
  -> MakeLogging i f
  -> i (TrackerCache f)
mkTrackerCache TrackerStoreSettings{..} tConfig MakeLogging{..} = do
  logging <- forComponent "PoolRepository"
  db      <- Rocks.open storePath
              Rocks.defaultOptions
                { Rocks.createIfMissing = createIfMissing
                }
  pure $ attachTracing logging TrackerCache
    { putMinIndex     = putMinIndex' db Rocks.defaultWriteOptions
    , getMinIndex     = getMinIndex' db Rocks.defaultReadOptions tConfig
    }

putMinIndex'
  :: (MonadIO f)
  => Rocks.DB
  -> Rocks.WriteOptions
  -> Gix
  -> f ()
putMinIndex' db opts newMinIndex =
  void $ liftIO $ Rocks.put db opts minIndexKey (BSU.fromString $ show newMinIndex)

getMinIndex'
  :: (MonadIO f)
  => Rocks.DB
  -> Rocks.ReadOptions
  -> TrackerProgrammConfig
  -> f Gix
getMinIndex' db opts TrackerProgrammConfig{..} =
  liftIO $ Rocks.get db opts minIndexKey <&> getCorrectIndex (Gix . naturalToInteger $ minIndex)

-- todo log err
getCorrectIndex :: Gix -> Maybe BS.ByteString -> Gix
getCorrectIndex defaultInput@(Gix defaultInputValue) input =
  case input of
    Nothing   -> defaultInput
    Just v ->
      let
        str = BSU.toString v
        minIndexInCache = read str :: Integer
        in
        if minIndexInCache < defaultInputValue then defaultInput else Gix minIndexInCache

attachTracing :: Monad f => Logging f -> TrackerCache f -> TrackerCache f
attachTracing Logging{..} TrackerCache{..} =
  TrackerCache
    { putMinIndex = \gix -> do
        debugM $ "putMinIndex " <> show gix
        r <- putMinIndex gix
        debugM $ "putMinIndex " <> show gix <> " -> " <> show r
        pure r
    , getMinIndex = do
        debugM @String "getMinIndex"
        r <- getMinIndex
        debugM $ "getMinIndex -> " <> show r
        pure r
    }
