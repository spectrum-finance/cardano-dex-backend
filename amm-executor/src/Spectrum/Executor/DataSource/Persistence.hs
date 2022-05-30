module Spectrum.Executor.DataSource.Persistence where

import Spectrum.Executor.Types (ConcretePoint)
import Control.Concurrent (newMVar, putMVar, readMVar)
import qualified Data.Map as Map
import Control.Monad.IO.Class (MonadIO (liftIO))

data Persistence m = Persistence
  { putLastPoint :: ConcretePoint -> m ()
  , getLastPoint :: m (Maybe ConcretePoint)
  }

mkRuntimePersistence :: MonadIO m => m (Persistence m)
mkRuntimePersistence = liftIO $ do
  store <- newMVar mempty
  pure Persistence
    { putLastPoint = \p -> liftIO $ do
        s <- readMVar store
        putMVar store $ Map.insert "k" p s
    , getLastPoint = liftIO $ do
        s <- readMVar store
        pure $ Map.lookup "k" s
    }
