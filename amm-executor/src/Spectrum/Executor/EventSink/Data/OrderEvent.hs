module Spectrum.Executor.EventSink.Data.OrderEvent
  ( OrderEvent(..)
  , type PendingOrderEvent
  , type PreEvaluatedOrderEvent
  , type EvaluatedOrderEvent
  , type CancelledOrderEvent
  ) where

import qualified Spectrum.Executor.Data.OrderState as State

data OrderEvent st ord where
  PendingOrder      :: ord -> OrderEvent 'State.Pending ord
  PreEvaluatedOrder :: ord -> OrderEvent 'State.PreEvaluated ord
  EvaluatedOrder    :: ord -> OrderEvent 'State.Evaluated ord
  CancelledOrder    :: ord -> OrderEvent 'State.Cancelled ord

type PendingOrderEvent ord = OrderEvent 'State.Pending ord

type PreEvaluatedOrderEvent ord = OrderEvent 'State.PreEvaluated ord

type EvaluatedOrderEvent ord = OrderEvent 'State.Evaluated ord

type CancelledOrderEvent ord = OrderEvent 'State.Cancelled ord
