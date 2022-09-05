module Spectrum.Common.Streaming.Class
  ( Compile(..)
  ) where

import qualified Streamly.Prelude as S
import Streamly.Prelude (SerialT)

-- | Compile stream to its effectful projection
class Compile s m where
  drain :: s m () -> m ()

instance Monad m => Compile SerialT m where
  drain = S.drain

