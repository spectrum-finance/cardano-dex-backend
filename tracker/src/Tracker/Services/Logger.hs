module Tracker.Services.Logger where

import Prelude
import RIO

log :: (MonadIO f) => String -> f ()
log = liftIO . print