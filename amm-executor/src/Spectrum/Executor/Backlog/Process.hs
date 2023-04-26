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

import Spectrum.Topic
  ( ReadTopic(..) )
import Spectrum.Executor.Data.OrderState
  ( OrderInState(EliminatedOrder), OrderState(Eliminated, Pending) )
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
  -> ReadTopic s m (OrderInState 'Pending)
  -> ReadTopic s m (OrderInState 'Eliminated)
  -> Backlog s m
mkBacklog backlogService pendingM pendingL eliminated =
  Backlog $
    S.parallel (trackPendingOrdersUpdates backlogService pendingL) $
    S.parallel (trackPendingOrdersUpdates backlogService pendingM)
    (trackEliminatedOrdersUpdates backlogService eliminated)

trackPendingOrdersUpdates
  :: (IsStream s, Monad (s m), Monad m)
  => BacklogService m
  -> ReadTopic s m (OrderInState 'Pending)
  -> s m ()
trackPendingOrdersUpdates BacklogService{..} ReadTopic{..} = do
  pendingOrder <- upstream
  S.fromEffect $ put pendingOrder

trackEliminatedOrdersUpdates
  :: (IsStream s, Monad m, Monad (s m))
  => BacklogService m
  -> ReadTopic s m (OrderInState 'Eliminated)
  -> s m ()
trackEliminatedOrdersUpdates BacklogService{..} ReadTopic{..} = do
  oId <- upstream <&> (\case EliminatedOrder oid -> oid)
  S.fromEffect $ drop oId