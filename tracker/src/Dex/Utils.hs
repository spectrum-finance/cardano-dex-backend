module Dex.Utils (unsafeFromEither) where

import RIO

unsafeFromEither :: Either String a -> a
unsafeFromEither (Left err)    = error err
unsafeFromEither (Right value) = value