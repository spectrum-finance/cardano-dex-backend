module Spectrum.Executor.PoolTracker.Process
  ( PoolTracker(..)
  , mkPoolTracker
  ) where

import qualified Streamly.Prelude as S
import Streamly.Prelude
  ( IsStream )

import Control.Monad.IO.Class
  ( MonadIO )
import Control.Monad.Trans.Control
  ( MonadBaseControl )
import Control.Monad.Catch
  ( MonadThrow )

import Spectrum.Executor.Data.PoolState
  ( Confirmed, Unconfirmed )
import Spectrum.Executor.Topic
  ( ReadTopic(..) )
import Spectrum.Executor.PoolTracker.Persistence.Pools
  ( Pools(..) )
import Spectrum.Executor.EventSink.Data.PoolEvent
  ( NewPool(..), DiscardedPool(..) )
import Spectrum.Common.Streaming.Class
  ( Compile(drain) )

newtype PoolTracker m = PoolTracker
  { run :: m ()
  }

mkPoolTracker
  :: ( IsStream s
     , Compile s m
     , Monad (s m)
     , MonadIO m
     , MonadBaseControl IO m
     , MonadThrow m
     )
  => Pools m
  -> ReadTopic s m (NewPool Confirmed)
  -> ReadTopic s m (NewPool Unconfirmed)
  -> ReadTopic s m DiscardedPool
  -> PoolTracker m
mkPoolTracker pools conf unconf discarded =
  PoolTracker . drain $
    S.parallel (trackConfirmedPoolUpdates pools conf) $
    S.parallel (trackUnconfirmedPoolUpdates pools unconf) $
    handlePoolRollbacks pools discarded

trackConfirmedPoolUpdates
  :: (IsStream s, Monad (s m), Monad m)
  => Pools m
  -> ReadTopic s m (NewPool Confirmed)
  -> s m ()
trackConfirmedPoolUpdates Pools{..} ReadTopic{..} = do
  NewPool pool <- upstream
  S.fromEffect $ putConfirmed pool

trackUnconfirmedPoolUpdates
  :: (IsStream s, Monad (s m), Monad m)
  => Pools m
  -> ReadTopic s m (NewPool Unconfirmed)
  -> s m ()
trackUnconfirmedPoolUpdates Pools{..} ReadTopic{..} = do
  NewPool pool <- upstream
  S.fromEffect $ putUnconfirmed pool

handlePoolRollbacks
  :: (IsStream s, Monad (s m), Monad m)
  => Pools m
  -> ReadTopic s m DiscardedPool
  -> s m ()
handlePoolRollbacks Pools{..} ReadTopic{..} = do
  DiscardedPool pid sid <- upstream
  S.fromEffect $ invalidate pid sid 
