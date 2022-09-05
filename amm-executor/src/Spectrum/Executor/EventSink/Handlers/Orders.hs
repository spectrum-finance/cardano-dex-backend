module Spectrum.Executor.EventSink.Handlers.Orders
  ( mkPendingOrdersHandler
  , mkEliminatedOrdersHandler
  ) where

import RIO
  ( (<&>), MonadIO )
import RIO.Time
  ( getCurrentTime )

import qualified Ledger as P
import qualified Data.Set as Set

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
import Spectrum.Executor.Types
  ( Order, OrderId (OrderId) )
import Spectrum.Executor.Data.OrderState
  ( OrderInState(PendingOrder, EliminatedOrder), OrderState(Pending, Eliminated) )
import Spectrum.Executor.EventSource.Data.TxContext
  ( TxCtx(LedgerCtx) )
import Spectrum.Executor.Backlog.Persistence.BacklogStore
  ( BacklogStore (BacklogStore, get) )

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

mkEliminatedOrdersHandler
  :: Monad m
  => BacklogStore m
  -> WriteTopic m (OrderInState 'Eliminated)
  -> EventHandler m 'LedgerCtx
mkEliminatedOrdersHandler BacklogStore{..} WriteTopic{..} = \case
  AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..}) -> do
      outs <- mapM tryProcessInputOrder (Set.toList txInputs)
      pure $ foldl (const id) Nothing outs
    where
      tryProcessInputOrder txin = do
          let orderId = OrderId $ P.txInRef txin
          maybeOrd <- get orderId
          case maybeOrd of
            Just _ -> publish (EliminatedOrder orderId) <&> Just
            _      -> pure Nothing
  _ -> pure Nothing
