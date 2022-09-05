{-# LANGUAGE DuplicateRecordFields #-}

module Spectrum.Executor.EventSource.Data.Tx
  ( MinimalUnconfirmedTx(..)
  , MinimalConfirmedTx(..)
  , MinimalTx(..)
  , fromBabbageLedgerTx
  ) where

import qualified Ledger as P
import qualified Data.Set as Set

import qualified Cardano.Ledger.Babbage.Tx as Al
import qualified Cardano.Crypto.Hash.Class as CC

import qualified PlutusTx.Prelude as PlutusTx

import CardanoTx.Models
  ( FullTxOut (FullTxOut), TxOutDatum (EmptyDatum, KnownDatumHash) )
import Spectrum.Executor.EventSource.Data.TxContext
  ( TxCtx(MempoolCtx, LedgerCtx) )
import Ouroboros.Consensus.Cardano.Block
    ( EraCrypto, StandardCrypto, BabbageEra)
import Ouroboros.Consensus.Shelley.Ledger (ShelleyHash (unShelleyHash))
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Ledger.Crypto (Crypto)

import qualified Ledger.Tx.CardanoAPI as Interop
import RIO ((<&>))
import Cardano.Api.Shelley (fromShelleyTxIn, fromShelleyTxOut, ShelleyBasedEra (ShelleyBasedEraBabbage))
import Data.Foldable (Foldable(toList))
import Cardano.Ledger.Serialization (Sized(sizedValue))

-- | A minimal sufficient representation of an unconfirmed transaction
data MinimalUnconfirmedTx = MinimalUnconfirmedTx
  { txId      :: P.TxId
  , txInputs  :: Set.Set P.TxIn
  , txOutputs :: [FullTxOut]
  } deriving (Eq, Show)

-- | A minimal sufficient representation of a confirmed transaction
data MinimalConfirmedTx = MinimalConfirmedTx
  { blockId   :: P.BlockId
  , txId      :: P.TxId
  , txInputs  :: Set.Set P.TxIn
  , txOutputs :: [FullTxOut]
  } deriving (Eq, Show)

data MinimalTx ctx where
  MinimalMempoolTx :: MinimalUnconfirmedTx -> MinimalTx 'MempoolCtx
  MinimalLedgerTx  :: MinimalConfirmedTx   -> MinimalTx 'LedgerCtx

deriving instance Eq (MinimalTx ctx)
deriving instance Show (MinimalTx ctx)

fromBabbageLedgerTx
  :: (Crypto crypto, crypto ~ StandardCrypto)
  => ShelleyHash (EraCrypto (BabbageEra crypto))
  -> Al.ValidatedTx (BabbageEra crypto) -> MinimalTx 'LedgerCtx
fromBabbageLedgerTx blockHash vtx =
  let
    body = Al.body vtx
    blockId
      = P.BlockId
      . CC.hashToBytes
      $ unShelleyHash blockHash
    txId
      = P.TxId
      . PlutusTx.toBuiltin
      . CC.hashToBytes
      . Ledger.extractHash
      . Ledger._unTxId
      . Ledger.txid
      $ body
    fromCardanoTxIn tin = P.TxIn (Interop.fromCardanoTxIn (fromShelleyTxIn tin)) Nothing
    fromCardanoTxOut ix tout =
      Interop.fromCardanoTxOut (fromShelleyTxOut ShelleyBasedEraBabbage (sizedValue tout)) <&> (\P.TxOut{..} ->
        FullTxOut
          (P.TxOutRef txId ix)
          txOutAddress
          txOutValue
          (maybe EmptyDatum KnownDatumHash txOutDatumHash))
  in MinimalLedgerTx $ MinimalConfirmedTx
    { blockId   = blockId
    , txId      = txId
    , txInputs  = Set.fromList $ Set.toList (Al.inputs body) <&> fromCardanoTxIn
    , txOutputs = zip [0..] (toList $ Al.outputs body)
                    <&> uncurry fromCardanoTxOut
                    >>= either mempty pure
    }
