module Spectrum.LedgerBridge where

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

import           Spectrum.LedgerBridge.Data.LedgerUpdate ( LedgerUpdate )
import qualified Spectrum.LedgerBridge.Data.LedgerUpdate as Update
import Spectrum.LedgerBridge.Protocol.Data.ChainSync
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

import Spectrum.LedgerBridge.Config
  ( BridgeConfig(..),
    NetworkParameters(NetworkParameters, slotsPerEpoch, networkMagic),
    ChainSyncClientConfig(ChainSyncClientConfig, maxInFlight, startAt) )
import Spectrum.LedgerBridge.Exception
  ( ChainSyncInitFailed(ChainSyncInitFailed) )
import Spectrum.LedgerBridge.Protocol.ChainSync
  ( mkChainSyncClient )
import Spectrum.LedgerBridge.Protocol.Client
  ( mkClient, connectClient, Block )

data LedgerBridge m block = LedgerBridge
  { pull    :: m (LedgerUpdate block)
  , tryPull :: m (Maybe (LedgerUpdate block))
  }

seeded
  :: forall m env.
    ( MonadAsync m
    , MonadST m
    , MonadThrow m
    , MonadResource m
    , MonadReader env m
    , HasType BridgeConfig env
    , HasType ChainSyncClientConfig env
    , HasType NetworkParameters env
    )
  => UnliftIO m
  -> Tracer m TraceClient
  -> m (LedgerBridge m Block)
seeded unliftIO tr = do
  BridgeConfig{nodeSocket}                      <- askContext
  ChainSyncClientConfig{maxInFlight, startAt}   <- askContext
  NetworkParameters{slotsPerEpoch,networkMagic} <- askContext
  (outQ, inQ) <- atomically $ (,) <$> newTQueue <*> newTQueue
  let
    chainSyncClient = mkChainSyncClient maxInFlight outQ inQ
    client          = mkClient unliftIO slotsPerEpoch chainSyncClient
    versions        = NodeToClientVersionData networkMagic
  connectClient (natTracer unliftIO tr) client versions nodeSocket
  seedTo outQ inQ startAt
  pure LedgerBridge
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
