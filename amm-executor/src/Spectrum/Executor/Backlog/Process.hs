module Spectrum.Executor.Backlog.Process 
  ( Backlog(..)
  , mkBacklog
  ) where

import qualified Streamly.Prelude as S
import Streamly.Prelude
  ( IsStream )

import Spectrum.Executor.Backlog.Service (BacklogService (BacklogService, put, drop))
import Spectrum.Executor.EventSource.Stream

import Control.Monad.IO.Class
  ( MonadIO )
import Control.Monad.Trans.Control
  ( MonadBaseControl )
import Control.Monad.Catch
  ( MonadThrow )
import Spectrum.Executor.Topic
  ( ReadTopic(..) )
import Spectrum.Common.Streaming.Class
  ( Compile(drain) )
import Spectrum.Executor.Data.OrderState (OrderInState, OrderState (Pending, Executed))
import Prelude hiding (drop)

newtype Backlog m = Backlog 
  { run :: m ()
  }

mkBacklog
  :: ( IsStream s
     , Compile s m
     , Monad (s m)
     , MonadIO m
     , MonadBaseControl IO m
     , MonadThrow m
     )
  => BacklogService m
  -> ReadTopic s m (OrderInState 'Pending)
  -> ReadTopic s m (OrderInState 'Executed)
  -> Backlog m
mkBacklog backlogService pending executed =
  Backlog . drain $
    S.parallel 
      (trackPendingOrdersUpdates backlogService pending)
      (trackExecutedOrdersUpdates backlogService executed) 

trackPendingOrdersUpdates
  :: (IsStream s, Monad (s m), Monad m)
  => BacklogService m
  -> ReadTopic s m (OrderInState 'Pending)
  -> s m ()
trackPendingOrdersUpdates BacklogService{..} ReadTopic{..} = do
  pendingOrder <- upstream
  S.fromEffect (put pendingOrder)

trackExecutedOrdersUpdates
  :: (IsStream s, Monad (s m), Monad m)
  => BacklogService m
  -> ReadTopic s m (OrderInState 'Executed)
  -> s m ()
trackExecutedOrdersUpdates BacklogService{..} ReadTopic{..} = do
  executedOrder <- upstream
  S.fromEffect (drop executedOrder)