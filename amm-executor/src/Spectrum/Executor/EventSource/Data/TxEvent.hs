module Spectrum.Executor.EventSource.Data.TxEvent
  ( TxEvent(..)
  ) where

import Spectrum.Executor.EventSource.Data.Tx
  ( MinimalTx )
import Spectrum.Executor.EventSource.Data.TxContext
  ( TxCtx(MempoolTx, LedgerTx) )
import qualified Ledger as P

data TxEvent ctx where
  PendingTx   :: MinimalTx 'MempoolTx -> TxEvent 'MempoolTx
  AppliedTx   :: MinimalTx 'LedgerTx  -> TxEvent 'LedgerTx
  UnappliedTx :: P.TxId -> TxEvent 'LedgerTx

deriving instance Eq (TxEvent ctx)
deriving instance Show (TxEvent ctx)
