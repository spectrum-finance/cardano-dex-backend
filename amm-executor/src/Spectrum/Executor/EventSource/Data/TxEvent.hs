module Spectrum.Executor.EventSource.Data.TxEvent where

import Spectrum.Executor.EventSource.Data.Tx
  ( MinimalTx )
import Spectrum.Executor.EventSource.Data.TxContext
  ( TxCtx(MempoolTx, LedgerTx) )

type TxHash = String

data TxEvent ctx where
  PendingTx   :: MinimalTx 'MempoolTx -> TxEvent 'MempoolTx
  AppliedTx   :: MinimalTx 'LedgerTx  -> TxEvent 'LedgerTx
  UpappliedTx :: TxHash -> TxEvent 'LedgerTx

deriving instance Eq (TxEvent ctx)
deriving instance Show (TxEvent ctx)
