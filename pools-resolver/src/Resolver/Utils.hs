module Resolver.Utils (unsafeFromEither) where

import RIO

unsafeFromEither :: Either b a -> a
unsafeFromEither (Left _)    = undefined
unsafeFromEither (Right value) = value