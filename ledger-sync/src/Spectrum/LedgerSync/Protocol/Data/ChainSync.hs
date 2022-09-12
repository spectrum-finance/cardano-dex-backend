{-# LANGUAGE DuplicateRecordFields #-}

module Spectrum.LedgerSync.Protocol.Data.ChainSync
  ( ChainSyncRequest(..)
  , ChainSyncResponse(..)
  , FindIntersect(..)
  , FindIntersectResponse(..)
  , RequestNext(..)
  , RequestNextResponse(..)
  ) where

import GHC.Generics
  ( Generic )

import Ouroboros.Network.Block
  ( Point (..), Tip (..) )

data ChainSyncRequest block
  = FindIntersectReq (FindIntersect block)
  | RequestNextReq RequestNext
  deriving (Generic, Show)
  
data ChainSyncResponse block
  = FindIntersectRes (FindIntersectResponse block)
  | RequestNextRes (RequestNextResponse block)
  deriving (Generic, Show)

data FindIntersect block
  = FindIntersect { points :: [Point block] }
  deriving (Generic, Show, Eq)

data FindIntersectResponse block
  = IntersectionFound { point :: Point block, tip :: Tip block }
  | IntersectionNotFound { tip :: Tip block }
  deriving (Generic, Show)

data RequestNext
  = RequestNext
  deriving (Generic, Show, Eq)

data RequestNextResponse block
  = RollForward { block :: block, tip :: Tip block }
  | RollBackward { point :: Point block, tip :: Tip block }
  deriving (Generic, Show)
