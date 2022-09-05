module Spectrum.Executor.EventSink.Handlers.Orders
  ( mkPendingOrdersHandler
  , mkExecutedOrdersHandler
  ) where

import RIO
  ( (<&>), MonadIO )
import Data.List as List
  ( find )

import Spectrum.Executor.EventSource.Data.TxEvent
  ( TxEvent (AppliedTx) )
import Spectrum.Executor.EventSink.Types
  ( EventHandler )
import Spectrum.Executor.EventSource.Data.Tx
  ( MinimalTx(MinimalLedgerTx), MinimalConfirmedTx (..) )
import Spectrum.Executor.Topic
  ( WriteTopic (..) )
import ErgoDex.Amm.Orders
  ( AnyOrder (AnyOrder)
  , Swap (swapPoolId)
  , Deposit (depositPoolId)
  , Redeem (redeemPoolId)
  , OrderAction (SwapAction, DepositAction, RedeemAction)
  )
import CardanoTx.Models
  ( FullTxOut (..) )
import ErgoDex.State
  ( OnChain (OnChain) )
import ErgoDex.Class
  ( FromLedger(parseFromLedger) )
import Spectrum.Executor.Types (Order, OrderId (OrderId))
import Spectrum.Executor.Data.OrderState
  ( OrderInState(PendingOrder, ExecutedOrder), OrderState(Pending, Executed) )
import RIO.Time (getCurrentTime)
import Spectrum.Executor.EventSource.Data.TxContext (TxCtx(LedgerCtx))
import Spectrum.Executor.Backlog.Persistence.BacklogStore (BacklogStore (BacklogStore, get))
import ErgoDex.Validators ( PoolValidator(..) )
import Plutus.V2.Ledger.Api (Credential(ScriptCredential), Address (Address))
import Ledger (validatorHash)
import qualified Ledger as P
import Data.Maybe (isJust)
import qualified Data.Set as Set

mkPendingOrdersHandler
  :: MonadIO m
  => WriteTopic m (OrderInState 'Pending)
  -> EventHandler m 'LedgerCtx
mkPendingOrdersHandler WriteTopic{..} = \case
  AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..}) ->
    foldl process (pure Nothing) (txOutputs <&> parseOrder)
      where
        process _ ordM = do
          ts <- getCurrentTime
          mapM publish $ ordM <&> flip PendingOrder ts
  _ -> pure Nothing

parseOrder :: FullTxOut -> Maybe Order
parseOrder out =
  let
    swap    = parseFromLedger @Swap out
    deposit = parseFromLedger @Deposit out
    redeem  = parseFromLedger @Redeem out
  in case (swap, deposit, redeem) of
    (Just (OnChain _ swap'), _, _)    -> Just . OnChain out $ AnyOrder (swapPoolId swap') (SwapAction swap')
    (_, Just (OnChain _ deposit'), _) -> Just . OnChain out $ AnyOrder (depositPoolId deposit') (DepositAction deposit')
    (_, _, Just (OnChain _ redeem'))  -> Just . OnChain out $ AnyOrder (redeemPoolId redeem') (RedeemAction redeem')
    _                                 -> Nothing

mkExecutedOrdersHandler
  :: Monad m
  => PoolValidator v
  -> BacklogStore m
  -> WriteTopic m (OrderInState 'Executed)
  -> EventHandler m 'LedgerCtx
mkExecutedOrdersHandler (PoolValidator pv) BacklogStore{..} WriteTopic{..} = \case
  AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..}) ->
      if isJust $ List.find containsPoolValidator txOutputs
        then do
          outs <- mapM tryProcessInputOrder (Set.toList txInputs)
          pure $ foldl (const id) Nothing outs
        else pure Nothing
    where
      containsPoolValidator FullTxOut{..} =
        case fullTxOutAddress of
          Address (ScriptCredential vh) _ -> validatorHash pv == vh
          _                               -> False
      tryProcessInputOrder txin = do
          let orderId = OrderId $ P.txInRef txin
          maybeOrd <- get orderId
          case maybeOrd of
            Just _ -> publish (ExecutedOrder orderId) <&> Just
            _      -> pure Nothing
  _ -> pure Nothing
