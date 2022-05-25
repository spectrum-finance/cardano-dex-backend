module Spectrum.LedgerSync
  ( LedgerSync(..)
  , mkLedgerSync
  ) where

import RIO ( (<&>) )

import Spectrum.Context ( MonadReader, HasType, askContext )
import Spectrum.Prelude ( UnliftIO )

import Control.Monad.Class.MonadSTM
  ( MonadSTM (..), MonadSTMTx (..), TQueue )
import Control.Monad.Class.MonadThrow
  ( MonadThrow (throwIO) )
import Control.Monad.Class.MonadST
  ( MonadST )
import Control.Monad.Class.MonadAsync
  ( MonadAsync )
import Control.Monad.Trans.Resource
  ( MonadResource )

import Control.Tracer
  ( Tracer (..), natTracer)

import           Spectrum.LedgerSync.Data.LedgerUpdate ( LedgerUpdate )
import qualified Spectrum.LedgerSync.Data.LedgerUpdate as Update
import Spectrum.LedgerSync.Protocol.Data.ChainSync
  ( RequestNextResponse(RollBackward, RollForward, block, point),
    RequestNext(RequestNext),
    ChainSyncResponse(RequestNextRes, FindIntersectRes),
    ChainSyncRequest(RequestNextReq, FindIntersectReq),
    FindIntersect(FindIntersect),
    FindIntersectResponse (IntersectionFound) )

import Ouroboros.Consensus.Block ( pointHash, StandardHash )
import Cardano.Network.Protocol.NodeToClient.Trace ( TraceClient )
import Ouroboros.Network.Block ( Point )
import Ouroboros.Network.NodeToClient.Version
  ( NodeToClientVersionData (NodeToClientVersionData) )

import Spectrum.LedgerSync.Config
  ( NetworkParameters(NetworkParameters, slotsPerEpoch, networkMagic),
    LedgerSyncConfig(..) )
import Spectrum.LedgerSync.Exception
  ( ChainSyncInitFailed(ChainSyncInitFailed) )
import Spectrum.LedgerSync.Protocol.ChainSync
  ( mkChainSyncClient )
import Spectrum.LedgerSync.Protocol.Client
  ( mkClient, connectClient, Block )
import Spectrum.LedgerSync.Types
  ( toPoint )

data LedgerSync m = LedgerSync
  { pull    :: m (LedgerUpdate Block)
  , tryPull :: m (Maybe (LedgerUpdate Block))
  }

mkLedgerSync
  :: forall m env.
    ( MonadAsync m
    , MonadST m
    , MonadThrow m
    , MonadResource m
    , MonadReader env m
    , HasType LedgerSyncConfig env
    , HasType NetworkParameters env
    )
  => UnliftIO m
  -> Tracer m TraceClient
  -> m (LedgerSync m)
mkLedgerSync unliftIO tr = do
  LedgerSyncConfig{nodeSocket, maxInFlight, startAt} <- askContext
  NetworkParameters{slotsPerEpoch,networkMagic}      <- askContext
  (outQ, inQ) <- atomically $ (,) <$> newTQueue <*> newTQueue
  let
    chainSyncClient = mkChainSyncClient maxInFlight outQ inQ
    client          = mkClient unliftIO slotsPerEpoch chainSyncClient
    versions        = NodeToClientVersionData networkMagic
  connectClient (natTracer unliftIO tr) client versions nodeSocket
  seedTo outQ inQ (toPoint startAt)
  pure LedgerSync
    { pull    = pull' outQ inQ
    , tryPull = tryPull' outQ inQ
    }

-- | Set chain sync state to the desired block
seedTo
  :: (MonadSTM m, MonadThrow m, StandardHash block)
  => TQueue m (ChainSyncRequest block)
  -> TQueue m (ChainSyncResponse block)
  -> Point block
  -> m ()
seedTo outQ inQ point = do
  atomically $ writeTQueue outQ $ FindIntersectReq $ FindIntersect [point]
  res <- atomically $ readTQueue inQ
  case res of
    FindIntersectRes (IntersectionFound _ _) -> pure ()
    _ -> throwIO $ ChainSyncInitFailed $ "An attempt to seed to an unknown point " <> show point

pull'
  :: MonadSTM m
  => TQueue m (ChainSyncRequest block)
  -> TQueue m (ChainSyncResponse block)
  -> m (LedgerUpdate block)
pull' outQ inQ = do
  atomically $ writeTQueue outQ $ RequestNextReq RequestNext
  atomically $ readTQueue inQ <&> extractUpdate

tryPull'
  :: MonadSTM m
  => TQueue m (ChainSyncRequest block)
  -> TQueue m (ChainSyncResponse block)
  -> m (Maybe (LedgerUpdate block))
tryPull' outQ inQ = do
  atomically $ writeTQueue outQ $ RequestNextReq RequestNext
  atomically $ tryReadTQueue inQ <&> (<&> extractUpdate)

extractUpdate :: ChainSyncResponse block -> LedgerUpdate block
extractUpdate (RequestNextRes RollForward{block})  = Update.RollForward block
extractUpdate (RequestNextRes RollBackward{point}) = Update.RollBackward $ pointHash point
extractUpdate _                                    = undefined
