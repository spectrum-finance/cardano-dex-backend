module Spectrum.LedgerBridge where

import RIO ((<&>), MonadReader)

import Spectrum.Context

import Control.Monad.Class.MonadSTM
  ( MonadSTM (..), MonadSTMTx (..), TQueue )
import Control.Monad.Class.MonadThrow
  ( MonadThrow )
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
import           Spectrum.LedgerBridge.Protocol.Data.ChainSync

import Ouroboros.Consensus.Block ( pointHash )
import Cardano.Network.Protocol.NodeToClient.Trace ( TraceClient )
import Ouroboros.Network.NodeToClient.Version
  ( NodeToClientVersionData (NodeToClientVersionData) )

import Spectrum.LedgerBridge.Config
import Spectrum.LedgerBridge.Protocol.ChainSync ( mkChainSyncClient )
import Spectrum.LedgerBridge.Protocol.Client    ( mkClient, connectClient, Block )

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
  => (forall a. m a -> IO a)
  -> Tracer m TraceClient
  -> m (LedgerBridge m Block)
seeded unliftIO tr = do
  BridgeConfig{nodeSock}                        <- askContext
  ChainSyncClientConfig{maxInFlight}            <- askContext
  NetworkParameters{slotsPerEpoch,networkMagic} <- askContext
  (outQ, inQ) <- atomically $ (,) <$> newTQueue <*> newTQueue
  let
    chainSyncClient = mkChainSyncClient maxInFlight outQ inQ
    client          = mkClient unliftIO slotsPerEpoch chainSyncClient
    versions        = NodeToClientVersionData networkMagic
  connectClient (natTracer unliftIO tr) client versions nodeSock
  pure LedgerBridge
    { pull    = pull' outQ inQ
    , tryPull = tryPull' outQ inQ
    }

pull' :: MonadSTM m => TQueue m (ChainSyncRequest block) -> TQueue m (ChainSyncResponse block) -> m (LedgerUpdate block)
pull' outQ inQ = do
  atomically $ writeTQueue outQ $ RequestNextReq RequestNext
  atomically $ readTQueue inQ <&> extractUpdate

tryPull' :: MonadSTM m => TQueue m (ChainSyncRequest block) -> TQueue m (ChainSyncResponse block) -> m (Maybe (LedgerUpdate block))
tryPull' outQ inQ = do
  atomically $ writeTQueue outQ $ RequestNextReq RequestNext
  atomically $ tryReadTQueue inQ <&> (<&> extractUpdate)

extractUpdate :: ChainSyncResponse block -> LedgerUpdate block
extractUpdate (RequestNextRes RollForward{block})  = Update.RollForward block
extractUpdate (RequestNextRes RollBackward{point}) = Update.RollBackward $ pointHash point
extractUpdate _                                    = undefined
