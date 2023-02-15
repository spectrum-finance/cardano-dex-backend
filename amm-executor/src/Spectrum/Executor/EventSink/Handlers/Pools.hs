module Spectrum.Executor.EventSink.Handlers.Pools
  ( mkNewPoolsHandler
  ) where

import RIO
  ( (<&>), MonadIO, foldM )

import ErgoDex.PValidators
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
  ( Logging(Logging, infoM) )
import CardanoTx.Models
  ( FullTxOut (FullTxOut, fullTxOutAddress) )
import ErgoDex.State
  ( OnChain )
import ErgoDex.Amm.Pool
  ( Pool )
import Plutus.Script.Utils.V2.Address
  ( mkValidatorAddress )

mkNewPoolsHandler
  :: MonadIO m
  => WriteTopic m (NewPool Confirmed)
  -> Logging m
  -> EventHandler m 'LedgerCtx
mkNewPoolsHandler WriteTopic{..} log@Logging{..} = \case
  AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..}) ->
    (parsePool log `traverse` txOutputs) >>= foldM process Nothing
      where process _ ordM = mapM publish $ ordM <&> NewPool
  _ -> pure Nothing

parsePool :: (MonadIO m) => Logging m -> FullTxOut -> m (Maybe (Confirmed (OnChain Pool)))
parsePool Logging{..} out@FullTxOut{..} = do
  pValidator <- poolValidator
  let
    pool        = parseFromLedger out
    poolAddress = mkValidatorAddress pValidator
  if fullTxOutAddress == poolAddress
    then case pool of
      Just a    -> do
        infoM ("Pool found in: " ++ show out)
        pure $ Just $ Confirmed a
      _         -> do
        infoM ("Pool not found in: " ++ show out)
        pure Nothing
  else pure Nothing
