module Spectrum.Executor.EventSource.LedgerView where

import Ouroboros.Consensus.Block.Abstract (SlotNo)

data LedgerView m = LedgerView
  { update :: SlotNo -> [String] -> m ()
  }
