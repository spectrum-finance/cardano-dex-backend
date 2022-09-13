module Spectrum.Executor.Topic
  ( ReadTopic(..)
  , WriteTopic(..)
  , OneToOneTopic(..)
  , mkOneToOneTopic
  , mkNoopTopic
  ) where

import Control.Concurrent.Chan.Unagi.NoBlocking
  ( newChan, readChan, writeChan, Next (Next, Pending), Stream (tryReadNext), streamChan, tryReadChan, Element (tryRead) )
import RIO
  ( MonadIO(..), MonadTrans (lift) )
import Streamly.Prelude as S
  ( IsStream, MonadAsync, repeatM, nil, fromPure, fromEffect )

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
    (ReadTopic . S.repeatM $ read outc)
    (WriteTopic $ liftIO . writeChan inc)
  where
    read chan = do
      elem <- liftIO $ tryReadChan chan
      a    <- liftIO $ tryRead elem
      case a of
        Just b  -> pure b
        Nothing -> read chan

mkNoopTopic
  :: forall s m a. (IsStream s, Applicative m)
  => (ReadTopic s m a, WriteTopic m a)
mkNoopTopic = (ReadTopic S.nil, WriteTopic . const . pure $ ())