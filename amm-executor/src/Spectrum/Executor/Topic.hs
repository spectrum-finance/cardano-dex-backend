module Spectrum.Executor.Topic
  ( ReadTopic(..)
  , WriteTopic(..)
  , OneToOneTopic(..)
  , mkOneToOneTopic
  , mkNoopTopic
  ) where

import qualified Control.Concurrent.Chan.Unagi as U

import Control.Concurrent.Chan.Unagi.NoBlocking
  ( newChan, readChan, writeChan, Next (Next, Pending), Stream (tryReadNext), streamChan, tryReadChan, Element (tryRead), isActive, InChan, OutChan)
import RIO
  ( MonadIO(..), MonadTrans (lift), MonadThrow (throwM), catch, SomeException, throwIO )
import Streamly.Prelude as S
  ( IsStream, MonadAsync, repeatM, nil, fromPure, fromEffect, after, before )
import Control.Exception 
  ( BlockedIndefinitelyOnMVar(BlockedIndefinitelyOnMVar) )
import System.Logging.Hlog
import Control.Concurrent 
  ( yield )

newtype ReadTopic s m a = ReadTopic
  { upstream :: s m a
  }

newtype WriteTopic m a = WriteTopic
  { publish :: a -> m ()
  }

data OneToOneTopic s m a = OneToOneTopic (ReadTopic s m a) (WriteTopic m a)

mkOneToOneTopic
  :: forall s f m a. (IsStream s, MonadAsync m, MonadIO f, MonadThrow m, Show a)
  => Logging m
  -> f (OneToOneTopic s m a)
mkOneToOneTopic logging@Logging{..} = liftIO $ do
  (inc, outc) <- newChan
  pure $ OneToOneTopic
    (ReadTopic . S.repeatM . liftIO $ readChan yield outc)
    (WriteTopic $ liftIO . writeChan inc)

mkNoopTopic
  :: forall s m a. (IsStream s, Applicative m)
  => (ReadTopic s m a, WriteTopic m a)
mkNoopTopic = (ReadTopic S.nil, WriteTopic . const . pure $ ())