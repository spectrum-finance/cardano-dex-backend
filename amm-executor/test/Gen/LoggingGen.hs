module Gen.LoggingGen where

import Control.Monad.IO.Class 
  ( MonadIO (liftIO) )
import Control.Monad 
  ( void )

import System.Logging.Hlog 
  ( MakeLogging (..), Logging (..), Loggable (..) )

mkMakeLogging :: MonadIO f => MakeLogging f f
mkMakeLogging = MakeLogging (const mkLogging)

mkLogging :: MonadIO f => f (Logging f)
mkLogging =
  pure $ Logging
    { debugM = \a -> liftIO $ print ("Debug: " ++ toLog a)
    , infoM  = \a -> liftIO $ print ("Info: " ++ toLog a)
    , warnM  = \a -> liftIO $ print ("Warn: " ++ toLog a)
    , errorM = \a -> liftIO $ print ("Error: " ++ toLog a)
    }

mkEmptyMakeLogging :: MonadIO f => MakeLogging f f
mkEmptyMakeLogging = MakeLogging (const mkEmptyLogging)

mkEmptyLogging :: Applicative f => f (Logging f)
mkEmptyLogging =
  pure $ Logging
    { debugM = \a -> pure ()
    , infoM  = \a -> pure ()
    , warnM  = \a -> pure ()
    , errorM = \a -> pure ()
    }
