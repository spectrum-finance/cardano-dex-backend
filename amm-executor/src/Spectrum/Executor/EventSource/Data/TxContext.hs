module Spectrum.Executor.EventSource.Data.TxContext
  ( TxCtx(..)
  ) where

data TxCtx = LedgerTx | MempoolTx
  deriving (Eq, Show)
