module Core.Utils where

import RIO

throwEither :: (MonadThrow f, Exception e) => Either e r -> f r
throwEither (Left err)    = throwM err
throwEither (Right value) = pure value

data MaybeErr = MaybeErr
  deriving Show

instance Exception MaybeErr

throwMaybe :: (MonadThrow f) => Maybe a -> f a
throwMaybe (Just value) = pure value
throwMaybe _ = throwM MaybeErr