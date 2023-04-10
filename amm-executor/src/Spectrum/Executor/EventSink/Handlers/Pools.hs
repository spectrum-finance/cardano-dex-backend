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
  ( Logging(Logging, infoM) )
import CardanoTx.Models
  ( FullTxOut (FullTxOut, fullTxOutAddress) )
import ErgoDex.State
  ( OnChain )
import ErgoDex.Amm.Pool
  ( Pool )
import Plutus.Script.Utils.V2.Address
  ( mkValidatorAddress )
import Spectrum.Executor.Scripts (ScriptsValidators (..))

mkNewPoolsHandler
  :: MonadIO m
  => WriteTopic m (NewPool Confirmed)
  -> Logging m
  -> ScriptsValidators
  -> EventHandler m 'LedgerCtx
mkNewPoolsHandler WriteTopic{..} log@Logging{..} validators = \case
  AppliedTx (MinimalLedgerTx MinimalConfirmedTx{..}) ->
    (parsePool log validators `traverse` txOutputs) >>= foldM process Nothing
      where process _ ordM = mapM publish $ ordM <&> NewPool
  _ -> pure Nothing

parsePool :: (MonadIO m) => Logging m -> ScriptsValidators -> FullTxOut -> m (Maybe (Confirmed (OnChain Pool)))
parsePool Logging{..} ScriptsValidators{poolValidator} out@FullTxOut{..} = do
  let
    pool        = parseFromLedger out
    poolAddress = mkValidatorAddress poolValidator
  if fullTxOutAddress == poolAddress
    then case pool of
      Just a    -> do
        infoM ("Pool found in: " ++ show out)
        pure $ Just $ Confirmed a
      _         -> do
        infoM ("Pool not found in: " ++ show out)
        pure Nothing
  else pure Nothing
