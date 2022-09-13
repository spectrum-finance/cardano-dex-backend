module Spectrum.Executor.Topic
  ( ReadTopic(..)
  , WriteTopic(..)
  , OneToOneTopic(..)
  , mkOneToOneTopic
  , mkNoopTopic
  ) where

import Control.Concurrent.Chan.Unagi.NoBlocking
  ( newChan, readChan, writeChan, Next (Next, Pending) )
import RIO
  ( MonadIO(..), MonadTrans (lift) )
import Streamly.Prelude as S
  ( IsStream, MonadAsync, repeatM, nil )

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
    (ReadTopic . S.repeatM . liftIO $ readChan (liftIO $ pure @IO ()) outc)
    (WriteTopic $ liftIO . writeChan inc)

mkNoopTopic
  :: forall s m a. (IsStream s, Applicative m)
  => (ReadTopic s m a, WriteTopic m a)
mkNoopTopic = (ReadTopic S.nil, WriteTopic . const . pure $ ())