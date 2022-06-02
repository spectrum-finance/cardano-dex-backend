module Spectrum.Executor.EventSource.Stream
  ( EventSource(..)
  , mkEventSource
  ) where

import RIO ( (&), MonadReader, (<&>), fromMaybe )

import Data.ByteString.Short (toShort)

import Streamly.Prelude as S

import System.Logging.Hlog
  ( MakeLogging(..), Logging(..) )

import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock(ShelleyBlock), ShelleyHash (unShelleyHash) )
import Ouroboros.Consensus.HardFork.Combinator
  ( OneEraHash(OneEraHash) )
import Ouroboros.Consensus.Cardano.Block
  ( HardForkBlock(BlockAlonzo) )

import Cardano.Ledger.Alonzo.TxSeq
  ( TxSeq(txSeqTxns) )
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Shelley.API as TPraos
import qualified Cardano.Crypto.Hash as CC

import Spectrum.LedgerSync.Protocol.Client
  ( Block )
import Spectrum.Executor.EventSource.Data.Tx
  ( fromAlonzoLedgerTx )
import Spectrum.LedgerSync
  ( LedgerSync(..) )
import Spectrum.Context
  ( HasType, askContext )
import Spectrum.Executor.Config
  ( EventSourceConfig (EventSourceConfig, startAt) )
import Spectrum.Executor.Types
  ( ConcretePoint (ConcretePoint), toPoint, ConcretePoint (slot), ConcreteHash (ConcreteHash) )
import Spectrum.Executor.EventSource.Persistence.LedgerHistory
  ( LedgerHistory (LedgerHistory, getLastPoint, putLastPoint), mkRuntimeLedgerHistory )
import Spectrum.Executor.EventSource.Data.TxEvent
  ( TxEvent(AppliedTx) )
import Spectrum.Executor.EventSource.Data.TxContext
import Spectrum.LedgerSync.Data.LedgerUpdate
  ( LedgerUpdate(RollForward) )

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
  persistence <- mkRuntimeLedgerHistory

  seekToBeginning logging persistence lsync startAt
  pure $ EventSource $ upstream' logging persistence lsync

upstream'
  :: forall s m. (IsStream s, Monad (s m), MonadAsync m)
  => Logging m
  -> LedgerHistory m
  -> LedgerSync m
  -> s m (TxEvent 'LedgerTx)
upstream' Logging{..} persistence LedgerSync{..}
  = S.repeatM pull >>= processUpdate persistence
  & S.trace (infoM . show)

processUpdate
  :: (IsStream s, Monad m)
  => LedgerHistory m
  -> LedgerUpdate Block
  -> s m (TxEvent 'LedgerTx)
processUpdate
  LedgerHistory{..}
  (RollForward (BlockAlonzo (ShelleyBlock (Ledger.Block (TPraos.BHeader hBody _) txs) hHash))) =
    let
      txs'  = txSeqTxns txs
      point = ConcretePoint (TPraos.bheaderSlotNo hBody) (ConcreteHash ch)
        where ch = OneEraHash . toShort . CC.hashToBytes . TPraos.unHashHeader . unShelleyHash $ hHash
    in S.before (putLastPoint point)
      $ S.fromFoldable txs' & S.map (AppliedTx . fromAlonzoLedgerTx hHash)
processUpdate _ _ = S.nil

seekToBeginning
  :: Monad m
  => Logging m
  -> LedgerHistory m
  -> LedgerSync m
  -> ConcretePoint
  -> m ()
seekToBeginning Logging{..} LedgerHistory{..} LedgerSync{..} pointLowConf = do
  lastCheckpoint <- getLastPoint
  let
    confSlot = slot pointLowConf
    pointLow = fromMaybe pointLowConf
      $ lastCheckpoint <&> (\p -> if confSlot > slot p then pointLowConf else p)
  infoM $ "Seeking to point " <> show pointLow
  seekTo $ toPoint pointLow
