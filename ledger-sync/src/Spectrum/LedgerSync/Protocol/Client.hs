module Spectrum.LedgerSync.Protocol.Client where

import Control.Monad.Class.MonadAsync
    ( MonadAsync )
import Control.Monad.Class.MonadST
    ( MonadST )
import Control.Monad.Class.MonadThrow
    ( MonadThrow )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Tracer
    ( Tracer (..), contramap, nullTracer )
import           Control.Monad.Class.MonadSTM

import Data.ByteString.Lazy
    ( ByteString )
import Data.Kind
    ( Type )
import Data.Map.Strict
    ( (!) )
import Data.Proxy
    ( Proxy (..) )
import Data.Void
    ( Void )

import Cardano.Chain.Slotting
    ( EpochSlots (..) )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Network.Protocol.NodeToClient.Trace
    ( TraceClient (..) )

import Network.Mux
    ( MuxMode (..), MiniProtocolNum (MiniProtocolNum), MiniProtocolLimits (MiniProtocolLimits, maximumIngressQueue) )
import Network.TypedProtocol.Codec
    ( Codec )
import Network.TypedProtocol.Codec.CBOR
    ( DeserialiseFailure )
import Ouroboros.Consensus.Byron.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoEras, CodecConfig (..) )
import Ouroboros.Consensus.Network.NodeToClient
    ( ClientCodecs, Codecs' (..), clientCodecs )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
    ( SupportedNetworkProtocolVersion (..) )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CodecConfig (..) )
import Ouroboros.Network.Block
    ( Point (..), Tip (..) )
import Ouroboros.Network.Channel
    ( Channel, hoistChannel )
import Ouroboros.Network.Driver.Simple
    ( TraceSendRecv, runPipelinedPeer )
import Ouroboros.Network.Mux
    ( MuxPeer (..), OuroborosApplication (..), RunMiniProtocol (..), ControlMessage, MiniProtocol (..) )
import Ouroboros.Network.NodeToClient
    ( LocalAddress
    , NetworkConnectTracers (..)
    , NodeToClientProtocols (..)
    , NodeToClientVersion (..)
    , NodeToClientVersionData (..)
    , connectTo
    , localSnocket
    , nodeToClientProtocols
    , withIOManager, ConnectionId
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( ChainSyncClientPipelined, chainSyncClientPeerPipelined )
import Ouroboros.Network.Protocol.ChainSync.Type
    ( ChainSync )
import Ouroboros.Network.Protocol.Handshake.Version
    ( combineVersions, simpleSingletonVersions )

-- | Concrete block type.
type Block = CardanoBlock StandardCrypto

-- | Concrete point type.
type ConcretePoint = Point Block

-- | Concrete eras type.
type Eras = CardanoEras StandardCrypto

-- | Type-family helper, similar to 'SubmitTxError' but more generic.
type family Crypto block :: Type where
    Crypto (CardanoBlock crypto) = crypto

-- | Type representing a network client running two mini-protocols to sync from the chain and, submit transactions.
type Client m = OuroborosApplication 'InitiatorMode LocalAddress ByteString m () Void

type ChainSyncClient m block = ChainSyncClientPipelined block (Point block) (Tip block) m ()

connectClient
  :: MonadIO m
  => Tracer IO TraceClient
  -> (NodeToClientVersion -> Client IO)
  -> NodeToClientVersionData
  -> FilePath
  -> m ()
connectClient tr mkClient' vData addr = liftIO $ withIOManager $ \iocp -> do
    connectTo (localSnocket iocp) tracers versions addr
  where
    versions = combineVersions
      [ simpleSingletonVersions v vData $ mkClient' v
      | v <- [NodeToClientV_10] -- todo: add v11, v12, v13
      ]
    tracers = NetworkConnectTracers
      { nctMuxTracer       = nullTracer
      , nctHandshakeTracer = contramap TrHandshake tr
      }

mkClient
  :: forall m.
      ( MonadAsync m
      , MonadIO m
      , MonadST m
      , MonadThrow m
      )
  => (forall a. m a -> IO a)
      -- ^ A natural transformation to unlift a particular 'm' into 'IO'.
  -> EpochSlots
      -- ^ Static blockchain parameters
  -> ChainSyncClient m Block
      -- ^ Client with the driving logic
  -> (NodeToClientVersion -> Client IO)
mkClient unlift epochSlots client = \nodeToClientV ->
    nodeToClientChainSync $ const $ pure $
      InitiatorProtocolOnly $ MuxPeerRaw $ \channel ->
        localChainSync unlift trChainSync (codecChainSync nodeToClientV)
        client
        (hoistChannel liftIO channel)
  where
    trChainSync    = nullTracer
    codecChainSync = cChainSyncCodec . codecs epochSlots

nodeToClientChainSync
  :: (ConnectionId addr -> STM m ControlMessage -> RunMiniProtocol appType bytes m a b)
  -> OuroborosApplication appType addr bytes m a b
nodeToClientChainSync protocols =
    OuroborosApplication $ \connectionId controlMessageSTM ->
      case protocols connectionId controlMessageSTM of
        localChainSyncProtocol ->
          [ localChainSyncMiniProtocol localChainSyncProtocol
          ]
  where
    maximumMiniProtocolLimits =
      MiniProtocolLimits
        { maximumIngressQueue = 0xffffffff
        }
    localChainSyncMiniProtocol localChainSyncProtocol =
      MiniProtocol
        { miniProtocolNum    = MiniProtocolNum 5
        , miniProtocolLimits = maximumMiniProtocolLimits
        , miniProtocolRun    = localChainSyncProtocol
        }

localChainSync
  :: forall m protocol.
      ( protocol ~ ChainSync Block (Point Block) (Tip Block)
      , MonadThrow m
      , MonadAsync m
      )
  => (forall a. m a -> IO a)
      -- ^ A natural transformation to unlift a particular 'm' into 'IO'.
  -> Tracer m (TraceSendRecv protocol)
      -- ^ Base tracer for the mini-protocols
  -> Codec protocol DeserialiseFailure m ByteString
      -- ^ Codec for deserializing / serializing binary data
  -> ChainSyncClient m Block
      -- ^ The actual chain sync client
  -> Channel m ByteString
      -- ^ A 'Channel' is a abstract communication instrument which
      -- transports serialized messages between peers (e.g. a unix
      -- socket).
  -> IO ((), Maybe ByteString)
localChainSync unliftIO tr codec client channel =
  unliftIO $ runPipelinedPeer tr codec channel (chainSyncClientPeerPipelined client)

codecs
  :: forall m. (MonadST m)
  => EpochSlots
  -> NodeToClientVersion
  -> ClientCodecs Block m
codecs epochSlots nodeToClientV =
    clientCodecs cfg (supportedVersions ! nodeToClientV) nodeToClientV
  where
    supportedVersions = supportedNodeToClientVersions (Proxy @Block)
    cfg = CardanoCodecConfig byron shelley allegra mary alonzo
      where
        byron   = ByronCodecConfig epochSlots
        shelley = ShelleyCodecConfig
        allegra = ShelleyCodecConfig
        mary    = ShelleyCodecConfig
        alonzo  = ShelleyCodecConfig
