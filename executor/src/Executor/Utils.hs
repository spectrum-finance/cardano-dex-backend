module Executor.Utils
    ( unsafeFromEither
    , unsafeFromMaybe
    ) where

import RIO

unsafeFromEither :: Either b a -> a
unsafeFromEither (Left err)    = undefined
unsafeFromEither (Right value) = value

unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe v =
    case v of
        Just v1 -> v1
        _ -> undefined