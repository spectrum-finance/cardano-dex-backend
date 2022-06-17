module Spectrum.Executor.Backlog.Service where

import qualified Database.RocksDB as Rocks

import Spectrum.Executor.Data.OrderState (OrderState (..), OrderInState (PendingOrder))
import Spectrum.Executor.Types (OrderId, Order, orderId)

import Spectrum.Common.Persistence.Serialization
  ( serialize, deserializeM )
import System.Logging.Hlog (MakeLogging(MakeLogging, forComponent))
import Spectrum.Executor.Backlog.Config (BacklogConfig(..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Aeson (FromJSON)
import RIO

data Backlog m = Backlog
  { put        :: OrderInState 'Pending -> m ()
  , suspend    :: OrderInState 'Suspended -> m Bool
  , checkLater :: OrderInState 'InProgress -> m Bool
  , tryAcquire :: m (Maybe Order)
  , drop       :: OrderId -> m Bool
  }

mkBacklog
  :: forall f m. (MonadIO f, MonadResource f, MonadIO m, MonadThrow m)
  => MakeLogging f m
  -> BacklogConfig
  -> f (Backlog m)
mkBacklog MakeLogging{..} BacklogConfig{..} = do
  logging <- forComponent "Pools"
  (_, db) <- Rocks.openBracket storePath
              Rocks.defaultOptions
                { Rocks.createIfMissing = createIfMissing
                }
  let
    get :: FromJSON a => ByteString -> m (Maybe a)
    get = (=<<) (mapM deserializeM) . Rocks.get db Rocks.defaultReadOptions
    put = Rocks.put db Rocks.defaultWriteOptions
    -- delete = Rocks.delete db Rocks.defaultWriteOptions
  pure Backlog
    { put = \(PendingOrder ord) -> do
        put (serialize $ orderId ord) (serialize ord)
        undefined
    }