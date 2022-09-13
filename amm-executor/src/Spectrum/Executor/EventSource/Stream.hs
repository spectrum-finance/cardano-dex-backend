module Spectrum.Executor.EventSource.Stream
  ( EventSource(..)
  , mkEventSource
  ) where

import RIO
  ( (&), MonadReader, (<&>), fromMaybe, ($>) )

import Data.ByteString.Short
  ( toShort )

import Control.Monad.Trans.Control
  ( MonadBaseControl )
import Control.Monad.IO.Class
  ( MonadIO )
import Control.Monad.Catch
  ( MonadThrow )
import Control.Monad
  ( join )
import Control.Monad.Trans.Resource
  ( MonadResource )

import Streamly.Prelude as S

import System.Logging.Hlog
  ( MakeLogging(..), Logging(..) )

import qualified Ouroboros.Consensus.Protocol.Praos.Header as Praos
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock(ShelleyBlock), ShelleyHash (unShelleyHash) )
import Ouroboros.Consensus.HardFork.Combinator
  ( OneEraHash(OneEraHash) )
import Ouroboros.Consensus.Cardano.Block
  ( HardForkBlock(BlockBabbage) )
import Ouroboros.Consensus.Block
  ( Point )

import Cardano.Ledger.Alonzo.TxSeq
  ( TxSeq(txSeqTxns) )
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Crypto.Hash as CC

import Spectrum.LedgerSync.Protocol.Client
  ( Block )
import Spectrum.Executor.EventSource.Data.Tx
  ( fromBabbageLedgerTx )
import Spectrum.LedgerSync
  ( LedgerSync(..) )
import Spectrum.Prelude.Context
  ( HasType, askContext )
import Spectrum.Executor.Config
  ( EventSourceConfig (EventSourceConfig, startAt) )
import Spectrum.Executor.EventSource.Types
  ( ConcretePoint (ConcretePoint)
  , toPoint
  , fromPoint
  , ConcretePoint (slot)
  , ConcreteHash (ConcreteHash)
  )
import Spectrum.Executor.EventSource.Persistence.LedgerHistory
  ( LedgerHistory (..), mkLedgerHistory )
import Spectrum.Executor.EventSource.Data.TxEvent
  ( TxEvent(AppliedTx, UnappliedTx) )
import Spectrum.Executor.EventSource.Data.TxContext
  ( TxCtx(LedgerCtx) )
import Spectrum.LedgerSync.Data.LedgerUpdate
  ( LedgerUpdate(RollForward, RollBackward) )
import Spectrum.Executor.EventSource.Persistence.Data.BlockLinks
  ( BlockLinks(BlockLinks, txIds, prevPoint) )
import Spectrum.Executor.EventSource.Persistence.Config
  ( LedgerStoreConfig )
import Spectrum.Prelude.HigherKind
  ( LiftK (liftK) )

newtype EventSource s m ctx = EventSource
  { upstream :: s m (TxEvent ctx)
  }

mkEventSource
  :: forall f m s env.
    ( Monad f
    , MonadResource f
    , LiftK m f
    , IsStream s
    , Monad (s m)
    , MonadAsync m
    , MonadReader env f
    , HasType (MakeLogging f m) env
    , HasType EventSourceConfig env
    , HasType LedgerStoreConfig env
    )
  => LedgerSync m
  -> f (EventSource s m 'LedgerCtx)
mkEventSource lsync = do
  mklog@MakeLogging{..}      <- askContext
  EventSourceConfig{startAt} <- askContext
  lhcong                     <- askContext

  logging     <- forComponent "EventSource"
  persistence <- mkLedgerHistory mklog lhcong

  liftK $ seekToBeginning logging persistence lsync startAt
  pure $ EventSource $ upstream' logging persistence lsync

upstream'
  :: forall s m. (IsStream s, Monad (s m), MonadAsync m)
  => Logging m
  -> LedgerHistory m
  -> LedgerSync m
  -> s m (TxEvent 'LedgerCtx)
upstream' logging@Logging{..} persistence LedgerSync{..}
  = S.repeatM pull >>= processUpdate logging persistence
  & S.trace (infoM . show)

processUpdate
  :: forall s m.
    ( IsStream s
    , Monad (s m)
    , MonadIO m
    , MonadBaseControl IO m
    , MonadThrow m
    )
  => Logging m
  -> LedgerHistory m
  -> LedgerUpdate Block
  -> s m (TxEvent 'LedgerCtx)
processUpdate
  _
  LedgerHistory{..}
  (RollForward (BlockBabbage (ShelleyBlock (Ledger.Block (Praos.Header hBody _) txs) hHash))) =
    let
      txs'  = txSeqTxns txs
      point = ConcretePoint (Praos.hbSlotNo hBody) (ConcreteHash ch)
        where ch = OneEraHash . toShort . CC.hashToBytes . unShelleyHash $ hHash
    in S.before (setTip point)
      $ S.fromFoldable txs' & S.map (AppliedTx . fromBabbageLedgerTx hHash)
processUpdate logging lh (RollBackward point) = streamUnappliedTxs logging lh point
processUpdate Logging{..} _ upd = S.before (errorM $ "Cannot process update " <> show upd) mempty

streamUnappliedTxs
  :: forall s m.
    ( IsStream s
    , Monad (s m)
    , MonadIO m
    , MonadBaseControl IO m
    , MonadThrow m
    )
  => Logging m
  -> LedgerHistory m
  -> Point Block
  -> s m (TxEvent 'LedgerCtx)
streamUnappliedTxs Logging{..} LedgerHistory{..} point = join $ S.fromEffect $ do
  knownPoint <- pointExists $ fromPoint point
  let
    rollbackOne :: ConcretePoint -> s m (TxEvent 'LedgerCtx)
    rollbackOne pt = do
      block <- S.fromEffect $ getBlock pt
      case block of
        Just BlockLinks{..} -> do
          S.fromEffect $ dropBlock pt >> setTip prevPoint
          let emitTxs = S.fromFoldable (Prelude.reverse txIds <&> UnappliedTx) -- unapply txs in reverse order
          if toPoint prevPoint == point
            then emitTxs
            else emitTxs <> rollbackOne prevPoint
        Nothing -> mempty
  tipM <- getTip
  case tipM of
    Just tip ->
      if knownPoint
        then infoM ("Rolling back to point " <> show point) $> rollbackOne tip
        else errorM ("An attempt to roll back to an unknown point " <> show point) $> mempty
    Nothing -> pure mempty

seekToBeginning
  :: Monad m
  => Logging m
  -> LedgerHistory m
  -> LedgerSync m
  -> ConcretePoint
  -> m ()
seekToBeginning Logging{..} LedgerHistory{..} LedgerSync{..} pointLowConf = do
  lastCheckpoint <- getTip
  let
    confSlot = slot pointLowConf
    pointLow = fromMaybe pointLowConf
      $ lastCheckpoint <&> (\p -> if confSlot > slot p then pointLowConf else p)
  infoM $ "Seeking to point " <> show pointLow
  seekTo $ toPoint pointLow
