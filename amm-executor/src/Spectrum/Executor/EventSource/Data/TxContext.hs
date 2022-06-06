module Spectrum.Executor.EventSource.Data.TxContext
  ( TxCtx(..)
  ) where

data TxCtx = LedgerCtx | MempoolCtx
  deriving (Eq, Show)
