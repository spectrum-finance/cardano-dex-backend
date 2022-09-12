module Spectrum.Executor.Topic
  ( ReadTopic(..)
  , WriteTopic(..)
  , OneToOneTopic(..)
  , mkOneToOneTopic
  , mkNoopTopic
  ) where

import Control.Concurrent.Chan.Unagi.NoBlocking
  ( newChan, readChan, writeChan, Next (Next, Pending), Stream (tryReadNext), streamChan, tryReadChan, Element (tryRead), isActive )
import RIO
  ( MonadIO(..), MonadTrans (lift), MonadThrow (throwM), catch, SomeException, throwIO )
import Streamly.Prelude as S
  ( IsStream, MonadAsync, repeatM, nil, fromPure, fromEffect )
import Control.Exception (BlockedIndefinitelyOnMVar(BlockedIndefinitelyOnMVar))

newtype ReadTopic s m a = ReadTopic
  { upstream :: s m a
  }

newtype WriteTopic m a = WriteTopic
  { publish :: a -> m ()
  }

data OneToOneTopic s m a = OneToOneTopic (ReadTopic s m a) (WriteTopic m a)

mkOneToOneTopic
  :: forall s f m a. (IsStream s, MonadAsync m, MonadIO f, MonadThrow m)
  => f (OneToOneTopic s m a)
mkOneToOneTopic = liftIO $ do
  (inc, outc) <- newChan
  pure $ OneToOneTopic
    (ReadTopic . S.repeatM . liftIO $ read outc)
    (WriteTopic $ liftIO . writeChan inc)
  where
    readChan1 io oc = tryReadChan oc >>= \el->
      let peekMaybe f = tryRead el >>= maybe f return
          go = peekMaybe checkAndGo
          checkAndGo = do
            b <- isActive oc
            if b then io >> go
                 -- Do a necessary final check of the element:
                 else (peekMaybe $ throwIO BlockedIndefinitelyOnMVar)
     in go
    withHandle chan =
      catch (readChan (pure ()) chan) (\(ex :: SomeException) -> liftIO (print $ "Ex:" ++ (show ex)) >> throwM ex)
    read chan = do
      elem <- liftIO $ tryReadChan chan
      a    <- liftIO $ tryRead elem
      case a of
        Just b  -> return b
        Nothing -> read chan

mkNoopTopic
  :: forall s m a. (IsStream s, Applicative m)
  => (ReadTopic s m a, WriteTopic m a)
mkNoopTopic = (ReadTopic S.nil, WriteTopic . const . pure $ ())