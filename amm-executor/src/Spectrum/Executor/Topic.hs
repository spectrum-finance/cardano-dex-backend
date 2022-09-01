module Spectrum.Executor.Topic
  ( ReadTopic(..)
  , WriteTopic(..)
  ) where

newtype ReadTopic s m a = ReadTopic
  { upstream :: s m a
  }

newtype WriteTopic m a = WriteTopic
  { publish :: a -> m ()
  }
