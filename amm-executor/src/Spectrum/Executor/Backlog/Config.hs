module Spectrum.Executor.Backlog.Config 
  ( BacklogServiceConfig (..)
  ) where

import RIO.Time (NominalDiffTime)
import Dhall (FromDhall, Generic)
import Dhall.Core
  ( Expr(..) )
import qualified Dhall as D
import GHC.Natural (naturalToInteger)

data BacklogServiceConfig = BacklogServiceConfig
  { orderLifetime :: NominalDiffTime
  , orderExecTime :: NominalDiffTime
  , pendingPropability :: Int 
  } deriving (Generic, FromDhall)

instance FromDhall NominalDiffTime where
  autoWith _ = D.Decoder{..}
    where
      extract (NaturalLit nat) = pure . fromInteger . naturalToInteger $ nat
      extract expr             = D.typeError expected expr

      expected = pure Natural
    