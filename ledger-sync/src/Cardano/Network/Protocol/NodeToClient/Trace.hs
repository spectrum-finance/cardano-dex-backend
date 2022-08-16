module Cardano.Network.Protocol.NodeToClient.Trace where

import Cardano.BM.Data.Severity
  ( Severity (..) )
import Cardano.BM.Data.Tracer
  ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Codec.CBOR.Term
  ( Term )
import Data.Aeson
  ( (.=) )
import GHC.Generics
  ( Generic )
import Network.Mux
  ( WithMuxBearer (..) )
import Network.TypedProtocol.Codec
  ( AnyMessageAndAgency (..) )
import Ouroboros.Network.Driver.Simple
  ( TraceSendRecv (..) )
import Ouroboros.Network.NodeToClient
  ( ConnectionId (..), LocalAddress, NodeToClientVersion )
import Ouroboros.Network.Protocol.Handshake.Type
  ( Handshake, Message (..), RefuseReason (..) )

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Map.Strict as Map

type HandshakeTrace = TraceSendRecv (Handshake NodeToClientVersion Term)

data TraceClient
  = TrHandshake (WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace)
  deriving (Generic, Show)

encodeTraceClient :: TraceClient -> Json.Value
encodeTraceClient= \case
    TrHandshake tr ->
      Json.object (("tag" .= Json.String "Handshake") : encodeTraceSendRecvHandshake tr)
  where
    encodeTraceSendRecvHandshake = \case
        WithMuxBearer _peerId (TraceSendMsg (AnyMessageAndAgency agency msg)) ->
          [ "event" .= ("send" :: String)
          , "agency" .= show agency
          ] ++ encodeMsg msg
        WithMuxBearer _peerId (TraceRecvMsg (AnyMessageAndAgency agency msg)) ->
          [ "event" .= ("receive" :: String)
          , "agency" .= show agency
          ] ++ encodeMsg msg
      where
        encodeMsg
          :: Message (Handshake NodeToClientVersion Term) from to
          -> [Json.Pair]
        encodeMsg = \case
          MsgProposeVersions versions ->
            [ "tag" .= ("ProposeVersions" :: String)
            , "versions" .= (show <$> Map.keys versions)
            ]
          MsgReplyVersions versions ->
            [ "tag" .= ("ReplyVersions" :: String)
            , "versions" .= (show <$> Map.keys versions)
            ]
          MsgAcceptVersion v _ ->
            [ "tag" .= ("AcceptVersion" :: String)
            , "version" .= show (show v)
            ]
          MsgRefuse reason ->
            [ "tag" .= ("RefuseVersions" :: String)
            , "reason" .= encodeRefuseReason reason
            ]

        encodeRefuseReason
          :: RefuseReason vNumber
          -> Json.Value
        encodeRefuseReason = \case
          VersionMismatch{} -> Json.String "VersionMismatchOrUnknown"
          HandshakeDecodeError{} -> Json.String "HandshakeDecodeError"
          Refused{} -> Json.String "ServerRejected"

instance HasPrivacyAnnotation TraceClient
instance HasSeverityAnnotation TraceClient where
  getSeverityAnnotation = \case
    TrHandshake{}    -> Info
