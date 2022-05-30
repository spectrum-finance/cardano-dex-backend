module Spectrum.Executor.DataSource.Stream
  ( DataSource(..)
  , mkDataSource
  ) where

import RIO ( (&), MonadReader, (<&>), fromMaybe )

import Streamly.Prelude as S
  ( repeatM, mapM, MonadAsync, IsStream )

import System.Logging.Hlog ( MakeLogging(..), Logging(..) )
import Spectrum.LedgerSync ( LedgerSync(..) )
import Spectrum.Context ( HasType, askContext )
import Spectrum.Executor.Config (DataSourceConfig (DataSourceConfig, startAt))
import Spectrum.Executor.Types (ConcretePoint, toPoint, ConcretePoint (slot))
import Spectrum.Executor.DataSource.Persistence (Persistence (Persistence, getLastPoint), mkRuntimePersistence)

newtype DataSource s m = DataSource
  { upstream :: s m ()
  }

mkDataSource
  :: forall m s env.
    ( IsStream s
    , MonadAsync m
    , MonadReader env m
    , HasType (MakeLogging m m) env
    , HasType DataSourceConfig env
    )
  => LedgerSync m
  -> m (DataSource s m)
mkDataSource lsync = do
  MakeLogging{..}           <- askContext
  DataSourceConfig{startAt} <- askContext

  logging     <- forComponent "DataSource"
  persistence <- mkRuntimePersistence

  initSync logging persistence lsync startAt
  pure $ DataSource $ upstream' logging lsync

upstream'
  :: forall s m. (IsStream s, MonadAsync m)
  => Logging m
  -> LedgerSync m
  -> s m ()
upstream' Logging{..} LedgerSync{..}
  = S.repeatM pull
  & S.mapM (infoM . show)

initSync :: Monad m => Logging m -> Persistence m -> LedgerSync m -> ConcretePoint -> m ()
initSync Logging{..} Persistence{..} LedgerSync{..} pointLowConf = do
  savedPoint <- getLastPoint
  let
    confSlot = slot pointLowConf
    pointLow = fromMaybe pointLowConf
      $ savedPoint <&> (\p -> if confSlot > slot p then pointLowConf else p)
  infoM $ "Seeking to point " <> show pointLow
  seekTo $ toPoint pointLow
