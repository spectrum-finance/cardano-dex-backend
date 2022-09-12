module Spectrum.Executor.EventSink.Handlers.Orders
  ( mkPendingOrdersHandler
  , mkEliminatedOrdersHandler
  ) where

import RIO
  ( (<&>), MonadIO (liftIO), foldM, QSem, signalQSem )
import RIO.Time
  ( getCurrentTime, secondsToNominalDiffTime, addUTCTime, diffUTCTime )

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
import Spectrum.LedgerSync.Config (NetworkParameters(NetworkParameters, systemStart))
import Cardano.Api (SlotNo(unSlotNo))
import Cardano.Slotting.Time (SystemStart(getSystemStart))
import Spectrum.Executor.Backlog.Config (BacklogServiceConfig(BacklogServiceConfig, orderLifetime))
import System.Logging.Hlog (Logging (Logging, infoM))

mkPendingOrdersHandler
  :: MonadIO m
  => WriteTopic m (OrderInState 'Pending)
  -> QSem
  -> Logging m
  -> BacklogServiceConfig
  -> NetworkParameters
  -> EventHandler m 'LedgerCtx
mkPendingOrdersHandler WriteTopic{..} syncSem logging@Logging{..} BacklogServiceConfig{..} NetworkParameters{..} = \case
  AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..}) -> do
    currentTime <- getCurrentTime
    let
       slotsTime = secondsToNominalDiffTime . fromIntegral $ unSlotNo slotNo
       txTime    = addUTCTime slotsTime (getSystemStart systemStart)
    if (diffUTCTime currentTime txTime > orderLifetime)
      then infoM ("Tx is outdated : " ++ show txId) >> pure Nothing
      else (parseOrder logging `traverse` txOutputs) >>= foldM (process txTime) Nothing           
      where
        process oTime _ ordM = (\order -> liftIO (signalQSem syncSem) >> publish order) `mapM` (ordM <&> flip PendingOrder oTime)
  _ -> pure Nothing

parseOrder :: (MonadIO m) => Logging m -> FullTxOut -> m (Maybe Order)
parseOrder Logging{..} out =
  let
    swap    = parseFromLedger @Swap out
    deposit = parseFromLedger @Deposit out
    redeem  = parseFromLedger @Redeem out
  in case (swap, deposit, redeem) of
    (Just (OnChain _ swap'), _, _)    -> do
      infoM ("Swap order: " ++ show swap)
      pure $ Just . OnChain out $ AnyOrder (swapPoolId swap') (SwapAction swap')
    (_, Just (OnChain _ deposit'), _) -> do
      infoM ("Deposit order: " ++ show deposit)
      pure $  Just . OnChain out $ AnyOrder (depositPoolId deposit') (DepositAction deposit')
    (_, _, Just (OnChain _ redeem'))  -> do
      infoM ("Redeem order: " ++ show redeem)
      pure $  Just . OnChain out $ AnyOrder (redeemPoolId redeem') (RedeemAction redeem')
    _                                 -> do
      infoM ("Order not found in: " ++ show out)
      pure $ Nothing

mkEliminatedOrdersHandler
  :: MonadIO m
  => BacklogStore m
  -> BacklogServiceConfig
  -> NetworkParameters
  -> WriteTopic m (OrderInState 'Eliminated)
  -> EventHandler m 'LedgerCtx
mkEliminatedOrdersHandler BacklogStore{..} BacklogServiceConfig{..} NetworkParameters{..} WriteTopic{..} = \case
  AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..}) -> do
      currentTime <- getCurrentTime
      let
        slotsTime = secondsToNominalDiffTime . fromIntegral $ unSlotNo slotNo
        txTime    = addUTCTime slotsTime (getSystemStart systemStart)
      if diffUTCTime currentTime txTime > orderLifetime
      then pure Nothing
      else do
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
