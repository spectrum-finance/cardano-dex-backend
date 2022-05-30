module Spectrum.LedgerSync
  ( LedgerSync(..)
  , mkLedgerSync
  ) where

import RIO ( (<&>), void )

import Spectrum.Context ( MonadReader, HasType, askContext )
import Spectrum.Prelude ( UnliftIO )

import GHC.Num ( naturalToInt )

import Control.Monad.Class.MonadSTM
  ( MonadSTM (..), MonadSTMTx (..), TQueue )
import Control.Monad.Class.MonadThrow
  ( MonadThrow (throwIO), MonadMask )
import Control.Monad.Class.MonadST
  ( MonadST )
import Control.Monad.Class.MonadAsync
  ( MonadAsync )
import Control.Monad.Trans.Resource
  ( MonadResource )
import Control.Monad.Class.MonadFork
  ( MonadFork (forkIO) )

import Control.Tracer
  ( Tracer (..), natTracer)

import System.Logging.Hlog
  ( Logging(Logging, errorM, warnM, infoM, debugM), MakeLogging(..) )

import Spectrum.LedgerSync.Data.LedgerUpdate
  ( LedgerUpdate )
import qualified Spectrum.LedgerSync.Data.LedgerUpdate as Update
import Spectrum.LedgerSync.Protocol.Data.ChainSync
  ( RequestNextResponse(RollBackward, RollForward, block, point),
    RequestNext(RequestNext),
    ChainSyncResponse(RequestNextRes, FindIntersectRes),
    ChainSyncRequest(RequestNextReq, FindIntersectReq),
    FindIntersect(FindIntersect),
    FindIntersectResponse (IntersectionFound) )

import Ouroboros.Consensus.Block
  ( StandardHash )
import Cardano.Network.Protocol.NodeToClient.Trace
  ( TraceClient )
import Ouroboros.Network.Block
  ( Point )
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

data LedgerSync m = LedgerSync
  { pull    :: m (LedgerUpdate Block)
  , tryPull :: m (Maybe (LedgerUpdate Block))
  , seekTo  :: Point Block -> m ()
  }

mkLedgerSync
  :: forall m env.
    ( MonadAsync m
    , MonadFork m
    , MonadMask m
    , MonadST m
    , MonadResource m
    , MonadReader env m
    , HasType LedgerSyncConfig env
    , HasType NetworkParameters env
    , HasType (MakeLogging m m) env
    )
  => UnliftIO m
  -> Tracer m TraceClient
  -> m (LedgerSync m)
mkLedgerSync unliftIO tr = do
  MakeLogging{..}                               <- askContext
  LedgerSyncConfig{nodeSocketPath, maxInFlight} <- askContext
  NetworkParameters{slotsPerEpoch,networkMagic} <- askContext

  Logging{..} <- forComponent "LedgerSync"
  (outQ, inQ) <- atomically $ (,) <$> newTQueue <*> newTQueue
  let
    chainSyncClient = mkChainSyncClient (naturalToInt maxInFlight) outQ inQ
    client          = mkClient unliftIO slotsPerEpoch chainSyncClient
    versions        = NodeToClientVersionData networkMagic
  
  infoM @String "Connecting Node Client"
  void $ forkIO $ connectClient (natTracer unliftIO tr) client versions nodeSocketPath
  infoM @String "LedgerSync initialized successfully"
  pure LedgerSync
    { pull    = pull' outQ inQ
    , tryPull = tryPull' outQ inQ
    , seekTo  = seekTo' outQ inQ
    }

-- | Set chain sync state to the desired block
seekTo'
  :: (MonadSTM m, MonadThrow m, StandardHash block)
  => TQueue m (ChainSyncRequest block)
  -> TQueue m (ChainSyncResponse block)
  -> Point block
  -> m ()
seekTo' outQ inQ point = do
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
extractUpdate (RequestNextRes RollBackward{point}) = Update.RollBackward point
extractUpdate _                                    = undefined
