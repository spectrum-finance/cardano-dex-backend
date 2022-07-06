module Gen.LoggingGen where
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Logging.Hlog (MakeLogging (MakeLogging), Logging (Logging, debugM, infoM, warnM, errorM), Loggable (toLog))
import Control.Monad (void)

mkMakeLogging :: MonadIO f => MakeLogging f f
mkMakeLogging = MakeLogging (const mkLogging)

mkLogging :: MonadIO f => f (Logging f)
mkLogging =
  pure $ Logging
    { debugM = \_ -> pure ()
    , infoM = \_ -> pure ()
    , warnM = \_ -> pure ()
    , errorM = \_ -> pure ()
    }