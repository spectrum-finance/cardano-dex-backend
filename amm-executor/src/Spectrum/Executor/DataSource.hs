module Spectrum.Executor.DataSource
  ( DataSource(..)
  , mkDataSource
  ) where

import RIO ( (&) )

import Streamly.Prelude as S
  ( repeatM, mapM, MonadAsync, IsStream )

import System.Logging.Hlog ( MakeLogging(..), Logging(..) )
import Spectrum.LedgerSync ( LedgerSync(..) )

newtype DataSource s m = DataSource
  { upstream :: s m ()
  }

mkDataSource
  :: (IsStream s, MonadAsync m)
  => MakeLogging m m
  -> LedgerSync m
  -> m (DataSource s m)
mkDataSource MakeLogging{..} lsync = do
  logging <- forComponent "DataSource"
  pure $ DataSource $ upstream' logging lsync

upstream'
  :: (IsStream s, MonadAsync m)
  => Logging m
  -> LedgerSync m
  -> s m ()
upstream' Logging{..} LedgerSync{..}
  = S.repeatM pull
  & S.mapM (infoM . show)
