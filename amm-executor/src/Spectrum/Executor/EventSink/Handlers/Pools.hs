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
import qualified ErgoDex.Amm.Pool as Core
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
  ( OnChain (OnChain) )
import Spectrum.Executor.Types
  ( Pool(..), PoolVersion (PoolVersionV1, PoolVersionV2) )
import Spectrum.Executor.Scripts (ScriptsValidators (..))
import Plutus.V2.Ledger.Api (addressCredential)
import ErgoDex.Validators (Version(V2, V1))

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

parsePool :: (MonadIO m) => Logging m -> ScriptsValidators -> FullTxOut -> m (Maybe (Confirmed Pool))
parsePool Logging{..} ScriptsValidators{poolV1Address, poolV2Address} out@FullTxOut{..} = do
  let
    pool        = parseFromLedger out
  case pool of
      Just a    -> do
        if addressCredential fullTxOutAddress == addressCredential poolV1Address
          then do
            infoM ("Pool v1 found in:" ++ show fullTxOutRef)
            pure $ Just $ Confirmed (Pool a V1)
          else if addressCredential fullTxOutAddress == addressCredential poolV2Address
            then do
              infoM ("Pool v2 found in: " ++ show fullTxOutRef)
              pure $ Just $ Confirmed (Pool a V2)
          else do
            debugM ("Pool not found in: " ++ show out)
            pure Nothing
      _         -> do
        debugM ("Pool not found in: " ++ show out)
        pure Nothing
