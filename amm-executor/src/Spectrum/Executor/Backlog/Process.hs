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
import Spectrum.Executor.Data.OrderState 
  ( OrderInState (ExecutedOrder, CancelledOrder), OrderState (Pending, Executed, Cancelled) )
import RIO ((<&>))

newtype Backlog s m = Backlog 
  { run :: s m ()
  }

mkBacklog
  :: ( IsStream s
     , Monad (s m)
     , MonadIO m
     , MonadBaseControl IO m
     , MonadThrow m
     )
  => BacklogService m
  -> ReadTopic s m (OrderInState 'Pending)
  -> ReadTopic s m (OrderInState 'Executed)
  -> ReadTopic s m (OrderInState 'Cancelled)
  -> Backlog s m
mkBacklog backlogService pending executed cancelled =
  Backlog $
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
  S.fromEffect $ put pendingOrder

trackExecutedOrdersUpdates
  :: (IsStream s, Monad m, Monad (s m))
  => BacklogService m
  -> ReadTopic s m (OrderInState 'Executed)
  -> s m ()
trackExecutedOrdersUpdates BacklogService{..} ReadTopic{..} = do
  oId <- upstream <&> (\case ExecutedOrder oid -> oid) 
  S.fromEffect $ drop oId

trackCancelledOrdersUpdates
  :: (IsStream s, Monad m, Monad (s m))
  => BacklogService m
  -> ReadTopic s m (OrderInState 'Cancelled)
  -> s m ()
trackCancelledOrdersUpdates BacklogService{..} ReadTopic{..} = do
  oId <- upstream <&> (\case CancelledOrder oid -> oid) 
  S.fromEffect $ drop oId