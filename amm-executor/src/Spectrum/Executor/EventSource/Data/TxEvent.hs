module Spectrum.Executor.EventSource.Data.TxEvent where

type TxHash = String

data Tx ctx = Tx String

data TxCtx = LedgerTx | MempoolTx
  deriving (Eq, Show)

data TxEvent ctx tx where
  PendingTx   :: tx     -> TxEvent 'MempoolTx tx
  AppliedTx   :: tx     -> TxEvent 'LedgerTx tx
  UpappliedTx :: TxHash -> TxEvent 'LedgerTx tx

deriving instance Eq tx => Eq (TxEvent ctx tx)
deriving instance Show tx => Show (TxEvent ctx tx)
