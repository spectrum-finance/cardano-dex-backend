module Spectrum.Executor.EventSink.Handlers.Orders
  ( mkPendingOrdersHandler
  ) where

import RIO
  ( (<&>) )

import Spectrum.Executor.EventSource.Data.TxEvent
  ( TxEvent (AppliedTx) )
import Spectrum.Executor.EventSink.Types
  ( EventHandler )
import Spectrum.Executor.EventSource.Data.Tx
  ( MinimalTx(MinimalLedgerTx), MinimalConfirmedTx (..) )
import Spectrum.Executor.EventSink.Data.OrderEvent
  ( PendingOrderEvent, OrderEvent (PendingOrder) )
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
  ( FullTxOut )
import ErgoDex.State
  ( OnChain (OnChain) )
import ErgoDex.Class
  ( FromLedger(parseFromLedger) )

mkPendingOrdersHandler
  :: Monad m
  => WriteTopic m (PendingOrderEvent (OnChain AnyOrder))
  -> EventHandler m ctx
mkPendingOrdersHandler WriteTopic{..} = \case 
  AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..}) ->
    foldl process (pure Nothing) (txOutputs <&> parseOrder)
      where process _ ordM = mapM publish $ ordM <&> PendingOrder
  _ -> pure Nothing

parseOrder :: FullTxOut -> Maybe (OnChain AnyOrder)
parseOrder out =
  let
    swap    = parseFromLedger @Swap out
    deposit = parseFromLedger @Deposit out
    redeem  = parseFromLedger @Redeem out
  in case (swap, deposit, redeem) of
    (Just (OnChain _ swap'), _, _)    -> Just . OnChain out $ AnyOrder (swapPoolId swap') (SwapAction swap')
    (_, Just (OnChain _ deposit'), _) -> Just . OnChain out $ AnyOrder (depositPoolId deposit') (DepositAction deposit')
    (_, _, Just (OnChain _ redeem'))  -> Just . OnChain out $ AnyOrder (redeemPoolId redeem') (RedeemAction redeem')
    _                     -> Nothing
