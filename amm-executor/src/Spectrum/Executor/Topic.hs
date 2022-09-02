module Spectrum.Executor.Topic
  ( ReadTopic(..)
  , WriteTopic(..)
  , OneToOneTopic(..)
  , mkOneToOneTopic
  ) where

import Control.Concurrent.Chan.Unagi.NoBlocking
  ( newChan, readChan, writeChan )
import RIO (MonadIO(..))
import Streamly.Prelude as S ( IsStream, MonadAsync, repeatM )

newtype ReadTopic s m a = ReadTopic
  { upstream :: s m a
  }

newtype WriteTopic m a = WriteTopic
  { publish :: a -> m ()
  }

data OneToOneTopic s m a = OneToOneTopic (ReadTopic s m a) (WriteTopic m a)

mkOneToOneTopic
  :: forall s f m a. (IsStream s, MonadAsync m, MonadIO f)
  => f (OneToOneTopic s m a)
mkOneToOneTopic = liftIO $ do
  (inc, outc) <- newChan
  pure $ OneToOneTopic
    (ReadTopic . S.repeatM . liftIO $ readChan (pure ()) outc)
    (WriteTopic $ liftIO . writeChan inc)
