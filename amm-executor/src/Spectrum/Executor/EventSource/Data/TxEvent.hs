module Spectrum.Executor.EventSource.Data.TxEvent
  ( TxEvent(..)
  ) where

import Spectrum.Executor.EventSource.Data.Tx
  ( MinimalTx )
import Spectrum.Executor.EventSource.Data.TxContext
  ( TxCtx(MempoolCtx, LedgerCtx) )
import qualified Ledger as P

data TxEvent ctx where
  PendingTx   :: MinimalTx 'MempoolCtx -> TxEvent 'MempoolCtx
  AppliedTx   :: MinimalTx 'LedgerCtx  -> TxEvent 'LedgerCtx
  UnappliedTx :: P.TxId -> TxEvent 'LedgerCtx

deriving instance Eq (TxEvent ctx)
deriving instance Show (TxEvent ctx)
