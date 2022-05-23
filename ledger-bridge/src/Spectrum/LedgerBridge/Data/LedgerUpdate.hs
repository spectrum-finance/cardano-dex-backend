module Spectrum.LedgerBridge.Data.LedgerUpdate where

import Ouroboros.Consensus.Block (ChainHash)

data LedgerUpdate block
  = RollForward block
  | RollBackward (ChainHash block)
