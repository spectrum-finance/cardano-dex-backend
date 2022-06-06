module Spectrum.Executor.Topic
  ( ReadTopic(..)
  , WriteTopic(..)
  ) where

newtype ReadTopic s m a = ReadTopic
  { subscribe :: s m a
  }

newtype WriteTopic m a = WriteTopic
  { publish :: a -> m ()
  }
