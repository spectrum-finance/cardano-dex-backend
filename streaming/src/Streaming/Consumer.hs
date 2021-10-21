module Streaming.Consumer where

import qualified Streamly.Prelude as S

data Consumer f k v = Consumer
  { upstream :: S.SerialT f (k, v)
  }
