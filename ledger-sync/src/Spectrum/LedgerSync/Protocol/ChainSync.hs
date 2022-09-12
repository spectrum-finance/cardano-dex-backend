module Spectrum.LedgerSync.Protocol.ChainSync
  ( mkChainSyncClient
  ) where

import RIO
  ( (<&>), ($>), liftIO )

import Control.Monad
  ( guard )
import Control.Monad.Class.MonadSTM
  ( MonadSTM (..), TQueue )

import Network.TypedProtocol.Pipelined
  ( Nat (..), natToInt )
import Ouroboros.Network.Block
  ( Point (..), Tip (..) )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
  ( ChainSyncClientPipelined (..)
  , ClientPipelinedStIdle (..)
  , ClientPipelinedStIntersect (..)
  , ClientStNext (..)
  )

import Spectrum.LedgerSync.Protocol.Data.ChainSync
  ( RequestNextResponse(RollBackward, RollForward)
  , RequestNext(RequestNext)
  , FindIntersectResponse(IntersectionNotFound, IntersectionFound)
  , FindIntersect(FindIntersect, points)
  , ChainSyncResponse(..)
  , ChainSyncRequest(..)
  )

type MaxInFlight = Int

mkChainSyncClient
  :: forall m block. MonadSTM m
  => MaxInFlight
  -- ^ Max number of requests allowed to be in-flight / pipelined
  -> TQueue m (ChainSyncRequest block)
  -- ^ Incoming request queue
  -> TQueue m (ChainSyncResponse block)
  -- ^ Outgoing response queue
  -> ChainSyncClientPipelined block (Point block) (Tip block) m ()
mkChainSyncClient maxInFlight incomingQ outgoingQ =
    ChainSyncClientPipelined $ clientStIdle Zero
  where
    pull :: m (ChainSyncRequest block)
    pull = atomically (readTQueue incomingQ)

    tryPull :: m (Maybe (ChainSyncRequest block))
    tryPull = atomically (tryReadTQueue incomingQ)

    clientStIdle :: Nat n -> m (ClientPipelinedStIdle n block (Point block) (Tip block) m ())
    clientStIdle Zero = pull <&> \case
      RequestNextReq RequestNext ->
        let 
          collect = CollectResponse
            (Just $ clientStIdle (Succ Zero))
            (clientStNext Zero)
        in SendMsgRequestNextPipelined collect

      FindIntersectReq FindIntersect{points} ->
        SendMsgFindIntersect points clientStIntersect

    clientStIdle n@(Succ prev) = tryPull >>= \case
      -- If there's no immediate incoming message, we take this opportunity to
      -- wait and collect one response.
      Nothing ->
        pure $ CollectResponse Nothing (clientStNext prev)

      -- Yet, if we have already received a new message from the client, we
      -- prioritize it and pipeline it right away unless there are already too
      -- many requests in flights.
      Just (RequestNextReq RequestNext) -> do
        let collect = CollectResponse (guard (natToInt n < maxInFlight) $> clientStIdle (Succ n)) (clientStNext n)
        pure $ SendMsgRequestNextPipelined collect

      Just (FindIntersectReq _FindIntersect) -> -- 'FindIntersect' requests cannot be interleaved with 'RequestNext'.
        clientStIdle n

    clientStNext :: Nat n -> ClientStNext n block (Point block) (Tip block) m ()
    clientStNext n =
      ClientStNext
        { recvMsgRollForward = \block tip -> do
            atomically $ writeTQueue outgoingQ $ RequestNextRes $ RollForward block tip
            clientStIdle n
        , recvMsgRollBackward = \point tip -> do
            atomically $ writeTQueue outgoingQ $ RequestNextRes $ RollBackward point tip
            clientStIdle n
        }

    clientStIntersect :: ClientPipelinedStIntersect block (Point block) (Tip block) m ()
    clientStIntersect = ClientPipelinedStIntersect
      { recvMsgIntersectFound = \point tip -> do
          atomically $ writeTQueue outgoingQ $ FindIntersectRes $ IntersectionFound point tip
          clientStIdle Zero
      , recvMsgIntersectNotFound = \tip -> do
          atomically $ writeTQueue outgoingQ $ FindIntersectRes $ IntersectionNotFound tip
          clientStIdle Zero
      }
