module Spectrum.Executor.Backlog.Process 
  ( Backlog(..)
  , mkBacklog
  ) where

import Prelude hiding (drop)
import Control.Monad.IO.Class
  ( MonadIO )
import Control.Monad.Trans.Control
  ( MonadBaseControl )
import Control.Monad.Catch
  ( MonadThrow )
import qualified Streamly.Prelude as S
import Streamly.Prelude
  ( IsStream )

import Spectrum.Executor.Backlog.Service (BacklogService (BacklogService, put, drop))

import Spectrum.Executor.Topic
  ( ReadTopic(..) )
import Spectrum.Common.Streaming.Class
  ( Compile(drain) )
import Spectrum.Executor.Data.OrderState 
  ( OrderInState (ExecutedOrder, CancelledOrder), OrderState (Pending, Executed, Cancelled) )

newtype Backlog m = Backlog 
  { run :: m ()
  }

mkBacklog
  :: ( IsStream s
     , Compile s m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadThrow m
     , MonadFail (s m)
     )
  => BacklogService m
  -> ReadTopic s m (OrderInState 'Pending)
  -> ReadTopic s m (OrderInState 'Executed)
  -> ReadTopic s m (OrderInState 'Cancelled)
  -> Backlog m
mkBacklog backlogService pending executed cancelled =
  Backlog . drain $
    S.parallel (trackPendingOrdersUpdates backlogService pending) $
    S.parallel (trackExecutedOrdersUpdates backlogService executed) $
    trackCancelledOrdersUpdates backlogService cancelled

trackPendingOrdersUpdates
  :: (IsStream s, Monad (s m), Monad m)
  => BacklogService m
  -> ReadTopic s m (OrderInState 'Pending)
  -> s m ()
trackPendingOrdersUpdates BacklogService{..} ReadTopic{..} = do
  pendingOrder <- upstream
  S.fromEffect (put pendingOrder)

trackExecutedOrdersUpdates
  :: (IsStream s, Monad m, MonadFail (s m))
  => BacklogService m
  -> ReadTopic s m (OrderInState 'Executed)
  -> s m ()
trackExecutedOrdersUpdates BacklogService{..} ReadTopic{..} = do
  ExecutedOrder oId <- upstream
  S.fromEffect $ drop oId

trackCancelledOrdersUpdates
  :: (IsStream s, Monad m, MonadFail (s m))
  => BacklogService m
  -> ReadTopic s m (OrderInState 'Cancelled)
  -> s m ()
trackCancelledOrdersUpdates BacklogService{..} ReadTopic{..} = do
  CancelledOrder oId <- upstream
  S.fromEffect $ drop oId