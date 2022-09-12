module Spectrum.Executor.EventSink.Handlers.Pools
  ( mkNewPoolsHandler
  ) where

import RIO
  ( (<&>), MonadIO, isJust, foldM )

import Spectrum.Executor.Topic
  ( WriteTopic (..) )
import Spectrum.Executor.Data.PoolState
  ( NewPool(..) )
import Spectrum.Executor.EventSink.Types
  ( EventHandler )
import Spectrum.Executor.EventSource.Data.TxEvent
  ( TxEvent(AppliedTx) )
import Spectrum.Executor.EventSource.Data.Tx
  ( MinimalTx(MinimalLedgerTx), MinimalConfirmedTx (..) )
import ErgoDex.Class
  ( FromLedger(parseFromLedger) )
import Spectrum.Executor.Data.State
  ( Confirmed(..) )
import Spectrum.Executor.EventSource.Data.TxContext
  ( TxCtx(LedgerCtx) )
import System.Logging.Hlog
  (Logging(Logging, infoM) )
import CardanoTx.Models (FullTxOut)
import ErgoDex.State (OnChain)
import ErgoDex.Amm.Pool (Pool)

mkNewPoolsHandler
  :: MonadIO m
  => WriteTopic m (NewPool Confirmed)
  -> Logging m
  -> EventHandler m 'LedgerCtx
mkNewPoolsHandler WriteTopic{..} log@Logging{..} = \case
  AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..}) ->
    (parsePool log `traverse` txOutputs) >>= foldM process Nothing
    -- (parsePool log `traverse` txOutputs) >>= (\poolsM -> do
    --     procRes <- process Nothing `traverse` poolsM
    --     let
    --       poolsFounded = length (isJust `filter` procRes) >= 1
    --       res = if poolsFounded then Just () else Nothing
    --     pure res
    --   )
      where process _ ordM = do
              res <- (parsePool log `traverse` txOutputs)
              infoM ("Processed pools (" ++ show txId ++ "): " ++ show res)
              infoM ("Pool (" ++ show txId ++ "): " ++ show ordM)
              mapM publish $ ordM <&> NewPool
  _ -> pure Nothing

parsePool :: (MonadIO m) => Logging m -> FullTxOut -> m (Maybe (Confirmed (OnChain Pool)))
parsePool Logging{..} out = do
  infoM ("In parse pool for " ++ show out)
  let
    pool    = parseFromLedger out
  case pool of
    Just a    -> do
      infoM ("pool: " ++ show pool)
      pure $ Just $ Confirmed a
    _                                 -> do
      infoM ("nothing pool: " ++ show out)
      pure $ Nothing
