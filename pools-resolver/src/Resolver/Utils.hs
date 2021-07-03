module Resolver.Utils (unsafeFromEither) where

import RIO

unsafeFromEither :: Either b a -> a
unsafeFromEither (Left err)    = undefined
unsafeFromEither (Right value) = value