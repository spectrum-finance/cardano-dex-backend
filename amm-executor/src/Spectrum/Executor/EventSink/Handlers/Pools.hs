module Spectrum.Executor.EventSink.Handlers.Pools
  ( mkNewPoolsHandler
  ) where

import RIO
  ( (<&>), MonadIO, foldM )

import Spectrum.Topic
  ( WriteTopic (..) )
import Spectrum.Executor.Data.PoolState
  ( NewPool(..) )
import Spectrum.Executor.EventSink.Types
  ( EventHandler )
import Spectrum.EventSource.Data.TxEvent
  ( TxEvent(AppliedTx) )
import Spectrum.EventSource.Data.Tx
  ( MinimalTx(MinimalLedgerTx), MinimalConfirmedTx (..) )
import ErgoDex.Class
  ( FromLedger(parseFromLedger) )
import Spectrum.Executor.Data.State
  ( Confirmed(..) )
import Spectrum.EventSource.Data.TxContext
  ( TxCtx(LedgerCtx) )
import System.Logging.Hlog
  ( Logging(Logging, infoM, debugM) )
import CardanoTx.Models
  ( FullTxOut (FullTxOut, fullTxOutAddress), fullTxOutRef )
import ErgoDex.State
  ( OnChain )
import ErgoDex.Amm.Pool
  ( Pool )
import Spectrum.Executor.Scripts (ScriptsValidators (..))
import Plutus.V2.Ledger.Api (addressCredential)

mkNewPoolsHandler
  :: MonadIO m
  => WriteTopic m (NewPool Confirmed)
  -> Logging m
  -> ScriptsValidators
  -> EventHandler m 'LedgerCtx
mkNewPoolsHandler WriteTopic{..} logging validators = \case
  AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..}) ->
    (parsePool logging validators `traverse` txOutputs) >>= foldM process Nothing
      where process _ ordM = mapM publish $ ordM <&> NewPool
  _ -> pure Nothing

parsePool :: (MonadIO m) => Logging m -> ScriptsValidators -> FullTxOut -> m (Maybe (Confirmed (OnChain Pool)))
parsePool Logging{..} ScriptsValidators{poolAddress} out@FullTxOut{..} = do
  let
    pool        = parseFromLedger out
  if addressCredential fullTxOutAddress == addressCredential poolAddress
    then case pool of
      Just a    -> do
        infoM ("Pool found in: " ++ show fullTxOutRef)
        pure $ Just $ Confirmed a
      _         -> do
        debugM ("Pool not found in: " ++ show out)
        pure Nothing
  else pure Nothing
