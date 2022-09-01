module Spectrum.LedgerSync.Data.LedgerUpdate where

import Ouroboros.Consensus.Block
  ( Point )

data LedgerUpdate block
  = RollForward block
  | RollBackward (Point block)
  deriving (Eq, Show)
