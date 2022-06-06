module Spectrum.Executor.Data.OrderState
  ( OrderState(..)
  ) where

data OrderState
  = Pending
  | PreEvaluated
  | Evaluated
  | Cancelled
  | Discarded
  deriving (Eq, Show)
