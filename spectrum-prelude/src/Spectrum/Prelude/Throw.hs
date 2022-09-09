module Spectrum.Prelude.Throw
  ( throwEither
  , throwMaybe
  ) where

import RIO
    ( Applicative(pure)
    , Maybe(Just)
    , Either(..)
    , MonadThrow(..)
    , Exception
    )

throwEither :: (MonadThrow f, Exception e) => Either e r -> f r
throwEither (Left err)    = throwM err
throwEither (Right value) = pure value

throwMaybe :: (MonadThrow f, Exception e) => e -> Maybe a -> f a
throwMaybe _ (Just value) = pure value
throwMaybe err _ = throwM err
