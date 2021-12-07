module Resolver.Utils
  ( unsafeFromEither
  ) where

import RIO
import Streaming.Events

unsafeFromEither :: Either b a -> a
unsafeFromEither (Left _)    = undefined
unsafeFromEither (Right value) = value