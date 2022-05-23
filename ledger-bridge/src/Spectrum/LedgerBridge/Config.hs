module Spectrum.LedgerBridge.Config
  ( ChainSyncClientConfig(..)
  , NetworkParameters(..)
  , BridgeConfig(..)
  ) where

import GHC.Generics ( Generic )

import Ouroboros.Consensus.Block ( Point )
import Cardano.Api               ( NetworkMagic, EpochSlots )
import Cardano.Slotting.Time     ( SystemStart )

import Spectrum.LedgerBridge.Protocol.Client ( Block )

data ChainSyncClientConfig = ChainSyncClientConfig
  { maxInFlight :: !Int
  , startAt     :: !(Point Block)
  }

data NetworkParameters = NetworkParameters
  { networkMagic  :: !NetworkMagic
  , systemStart   :: !SystemStart
  , slotsPerEpoch :: !EpochSlots
  } deriving (Generic, Eq, Show)

data BridgeConfig = BridgeConfig
  { nodeSock  :: FilePath
  }
