module Spectrum.Executor.DataSource
  ( DataSource(..)
  , mkDataSource
  ) where

import RIO ( (&), MonadReader )

import Streamly.Prelude as S
  ( repeatM, mapM, MonadAsync, IsStream )

import System.Logging.Hlog ( MakeLogging(..), Logging(..) )
import Spectrum.LedgerSync ( LedgerSync(..) )
import Spectrum.Context ( HasType, askContext )

newtype DataSource s m = DataSource
  { upstream :: s m ()
  }

mkDataSource
  :: forall m s env.
    ( IsStream s
    , MonadAsync m
    , MonadReader env m
    , HasType (MakeLogging m m) env
    )
  => LedgerSync m
  -> m (DataSource s m)
mkDataSource lsync = do
  MakeLogging{..} <- askContext
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
