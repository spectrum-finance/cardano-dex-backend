module Spectrum.Executor.EventSink.Types
  ( type EventHandler
  , type DefaultEventHandler
  ) where

import Spectrum.Executor.EventSource.Data.TxEvent
  ( TxEvent )

type EventHandler m ctx = TxEvent ctx -> m (Maybe ())

type DefaultEventHandler m ctx = TxEvent ctx -> m ()
