module Spectrum.LedgerSync
  ( LedgerSync(..)
  , mkLedgerSync
  ) where

import RIO ( (<&>), void )

import Spectrum.Prelude.Context
  ( MonadReader, HasType, askContext )
import Spectrum.Prelude.UnliftIO
  ( UnliftIO )

import GHC.Num ( naturalToInt )

import Control.Monad.IO.Class
  ( MonadIO )
import Control.Monad.Class.MonadSTM
  ( MonadSTM (..), TQueue )
import Control.Monad.Class.MonadThrow
  ( MonadThrow (throwIO), MonadMask )
import Control.Monad.Class.MonadST
  ( MonadST )
import Control.Monad.Class.MonadAsync
  ( MonadAsync )
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
import Spectrum.Prelude.HigherKind
  ( FunctorK(..) )

data LedgerSync m = LedgerSync
  { pull    :: m (LedgerUpdate Block)
  , tryPull :: m (Maybe (LedgerUpdate Block))
  , seekTo  :: Point Block -> m ()
  }

instance FunctorK LedgerSync where
  fmapK trans LedgerSync{..} =
    LedgerSync
      { pull    = trans pull
      , tryPull = trans tryPull
      , seekTo  = trans . seekTo
      }

mkLedgerSync
  :: forall m env.
    ( MonadAsync m
    , MonadFork m
    , MonadMask m
    , MonadST m
    , MonadIO m
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

  l@Logging{..} <- forComponent "LedgerSync"
  (outQ, inQ) <- atomically $ (,) <$> newTQueue <*> newTQueue
  let
    chainSyncClient = mkChainSyncClient (naturalToInt maxInFlight) outQ inQ
    client          = mkClient unliftIO slotsPerEpoch chainSyncClient
    versions        = NodeToClientVersionData networkMagic

  infoM @String "Connecting Node Client"
  void $ forkIO $ connectClient (natTracer unliftIO tr) client versions nodeSocketPath
  infoM @String "LedgerSync initialized successfully"
  pure LedgerSync
    { pull    = pull' l outQ inQ
    , tryPull = tryPull' outQ inQ
    , seekTo  = seekTo' l outQ inQ
    }

-- | Set chain sync state to the desired block
seekTo'
  :: (MonadSTM m, MonadThrow m, StandardHash block)
  => Logging m
  -> TQueue m (ChainSyncRequest block)
  -> TQueue m (ChainSyncResponse block)
  -> Point block
  -> m ()
seekTo' Logging{..} outQ inQ point = do
  atomically $ writeTQueue outQ $ FindIntersectReq $ FindIntersect [point]
  res <- atomically $ readTQueue inQ
  case res of
    FindIntersectRes (IntersectionFound _ _) -> 
      infoM @String"IntersectionFound!" >> pure ()
    _ -> 
      infoM @String"An attempt to seed to an unknown point" >> (throwIO $ ChainSyncInitFailed $ "An attempt to seed to an unknown point " <> show point)

pull'
  :: MonadSTM m
  => Logging m
  -> TQueue m (ChainSyncRequest block)
  -> TQueue m (ChainSyncResponse block)
  -> m (LedgerUpdate block)
pull' Logging{..} outQ inQ = do
  infoM @String "Going to pull'"
  atomically $ writeTQueue outQ $ RequestNextReq RequestNext
  infoM @String "Waiting pull result"
  result <- atomically $ readTQueue inQ <&> extractUpdate
  case result of
    (Update.RollForward _) ->  infoM @String "Pull result: RollForward"
    (Update.RollBackward _) -> infoM @String "Pull result: RollBackward"
  pure result

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
