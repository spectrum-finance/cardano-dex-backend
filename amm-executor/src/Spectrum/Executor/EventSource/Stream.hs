module Spectrum.Executor.EventSource.Stream
  ( EventSource(..)
  , mkEventSource
  ) where

import RIO ( (&), MonadReader, (<&>), fromMaybe )

import Streamly.Prelude as S

import System.Logging.Hlog ( MakeLogging(..), Logging(..) )
import Spectrum.LedgerSync ( LedgerSync(..) )
import Spectrum.Context ( HasType, askContext )
import Spectrum.Executor.Config (EventSourceConfig (EventSourceConfig, startAt))
import Spectrum.Executor.Types (ConcretePoint, toPoint, ConcretePoint (slot))
import Spectrum.Executor.EventSource.Persistence (Persistence (Persistence, getLastPoint), mkRuntimePersistence)
import Spectrum.Executor.EventSource.Data.TxEvent
import Spectrum.Executor.EventSource.Data.TxContext
import Spectrum.LedgerSync.Data.LedgerUpdate (LedgerUpdate(RollForward))
import Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock(ShelleyBlock))

import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import Spectrum.LedgerSync.Protocol.Client (Block)
import Ouroboros.Consensus.Cardano.Block (HardForkBlock(BlockAlonzo), AlonzoEra, StandardCrypto)
import Cardano.Ledger.Alonzo.Tx (ValidatedTx)
import Cardano.Ledger.Alonzo.TxSeq (TxSeq(txSeqTxns))
import Spectrum.Executor.EventSource.Data.Tx (fromAlonzoLedgerTx)

newtype EventSource s m = EventSource
  { upstream :: s m (TxEvent 'LedgerTx)
  }

mkEventSource
  :: forall m s env.
    ( IsStream s
    , Monad (s m)
    , MonadAsync m
    , MonadReader env m
    , HasType (MakeLogging m m) env
    , HasType EventSourceConfig env
    )
  => LedgerSync m
  -> m (EventSource s m)
mkEventSource lsync = do
  MakeLogging{..}            <- askContext
  EventSourceConfig{startAt} <- askContext

  logging     <- forComponent "EventSource"
  persistence <- mkRuntimePersistence

  seekToBeginning logging persistence lsync startAt
  pure $ EventSource $ upstream' logging lsync

upstream'
  :: forall s m. (IsStream s, Monad (s m), MonadAsync m)
  => Logging m
  -> LedgerSync m
  -> s m (TxEvent 'LedgerTx)
upstream' Logging{..} LedgerSync{..}
  = S.repeatM pull >>= processUpdate & S.trace (infoM . show)

processUpdate :: (IsStream s, Monad m) => LedgerUpdate Block -> s m (TxEvent 'LedgerTx)
processUpdate (RollForward (BlockAlonzo (ShelleyBlock (Ledger.Block _ txs) headerHash))) =
  let txs' = txSeqTxns txs
  in S.fromFoldable txs' & S.map (AppliedTx . fromAlonzoLedgerTx headerHash)
processUpdate _ = S.nil

seekToBeginning :: Monad m => Logging m -> Persistence m -> LedgerSync m -> ConcretePoint -> m ()
seekToBeginning Logging{..} Persistence{..} LedgerSync{..} pointLowConf = do
  lastCheckpoint <- getLastPoint
  let
    confSlot = slot pointLowConf
    pointLow = fromMaybe pointLowConf
      $ lastCheckpoint <&> (\p -> if confSlot > slot p then pointLowConf else p)
  infoM $ "Seeking to point " <> show pointLow
  seekTo $ toPoint pointLow
