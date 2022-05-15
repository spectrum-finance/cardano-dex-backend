module Spectrum.LedgerBridge.Protocol.Data.ChainSync where

import Ouroboros.Network.Block
    ( Point (..), Tip (..) )

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
