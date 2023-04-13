module Spectrum.Executor.EventSink.Pipe
  ( EventSink(..)
  , mkEventSink
  ) where

import Data.Maybe
  ( isJust )
import Data.List
  ( findIndex )

import qualified Streamly.Prelude as S
import Streamly.Prelude
  ( MonadAsync, IsStream )

import Spectrum.EventSource.Data.TxEvent
  ( TxEvent )
import Spectrum.Executor.EventSink.Types
  ( EventHandler, DefaultEventHandler )

newtype EventSink s m ctx = EventSink
  { pipe :: s m (TxEvent ctx) -> s m ()
  }

mkEventSink
  :: (IsStream s, MonadAsync m)
  => [EventHandler m ctx]
  -> DefaultEventHandler m ctx
  -> EventSink s m ctx
mkEventSink handlers onUnhandeled =
  EventSink
    { pipe = pipe' handlers onUnhandeled 
    }

pipe'
  :: (IsStream s, MonadAsync m)
  => [EventHandler m ctx]
  -> DefaultEventHandler m ctx
  -> s m (TxEvent ctx)
  -> s m ()
pipe' handlers onUnhandeled =
    S.mapM handle
  where
    handle e =
      mapM ($ e) handlers >>= (\xs ->
        if isJust $ findIndex isJust xs
          then pure ()
          else onUnhandeled e)
