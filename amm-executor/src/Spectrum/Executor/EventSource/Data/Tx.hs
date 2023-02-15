{-# LANGUAGE DuplicateRecordFields #-}

module Spectrum.Executor.EventSource.Data.Tx
  ( MinimalUnconfirmedTx(..)
  , MinimalConfirmedTx(..)
  , MinimalTx(..)
  , fromBabbageLedgerTx
  ) where

import RIO 
  ( (<&>) )
import Data.Foldable 
  ( Foldable(toList) )

import qualified Ledger as P
import qualified Data.Set as Set

import qualified Cardano.Ledger.Babbage.Tx as Al
import qualified Cardano.Crypto.Hash.Class as CC
import qualified Cardano.Ledger.SafeHash   as Ledger
import qualified Cardano.Ledger.TxIn       as Ledger

import qualified PlutusTx.Prelude as PlutusTx

import CardanoTx.Models
  ( FullTxOut (FullTxOut), TxOutDatum (EmptyDatum, KnownDatumHash, KnownDatum) )
import Cardano.Api.Shelley 
  ( fromShelleyTxIn, fromShelleyTxOut, ShelleyBasedEra (ShelleyBasedEraBabbage), SlotNo )
import Spectrum.Executor.EventSource.Data.TxContext
  ( TxCtx(MempoolCtx, LedgerCtx) )
import Ouroboros.Consensus.Cardano.Block
  ( EraCrypto, StandardCrypto, BabbageEra)
import Ouroboros.Consensus.Shelley.Ledger 
  ( ShelleyHash (unShelleyHash) )
import Cardano.Ledger.Crypto 
  ( Crypto )
import Cardano.Ledger.Serialization 
  ( Sized(sizedValue) )

import qualified Ledger.Tx.CardanoAPI as Interop
import qualified CardanoTx.Interop    as Interop
import qualified Plutus.V2.Ledger.Tx  as PV2

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
  , slotNo    :: SlotNo
  } deriving (Eq, Show)

data MinimalTx ctx where
  MinimalMempoolTx :: MinimalUnconfirmedTx -> MinimalTx 'MempoolCtx
  MinimalLedgerTx  :: MinimalConfirmedTx   -> MinimalTx 'LedgerCtx

deriving instance Eq (MinimalTx ctx)
deriving instance Show (MinimalTx ctx)

fromBabbageLedgerTx
  :: (Crypto crypto, crypto ~ StandardCrypto)
  => ShelleyHash (EraCrypto (BabbageEra crypto))
  -> SlotNo
  -> Al.ValidatedTx (BabbageEra crypto) -> MinimalTx 'LedgerCtx
fromBabbageLedgerTx blockHash slotNo vtx =
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
      Interop.fromCardanoTxOutV2 (fromShelleyTxOut ShelleyBasedEraBabbage (sizedValue tout)) <&> (\PV2.TxOut{..} ->
        FullTxOut
          (P.TxOutRef txId ix)
          txOutAddress
          txOutValue
          (parseDatum txOutDatum)
          txOutReferenceScript)
    parseDatum datum = case datum of
      PV2.NoOutputDatum      -> EmptyDatum
      PV2.OutputDatumHash dh -> KnownDatumHash dh
      PV2.OutputDatum d      -> KnownDatum d
  in MinimalLedgerTx $ MinimalConfirmedTx
    { blockId   = blockId
    , txId      = txId
    , txInputs  = Set.fromList $ Set.toList (Al.inputs body) <&> fromCardanoTxIn
    , txOutputs = zip [0..] (toList $ Al.outputs body)
                    <&> uncurry fromCardanoTxOut
                    >>= either mempty pure
    , slotNo    = slotNo
    }
