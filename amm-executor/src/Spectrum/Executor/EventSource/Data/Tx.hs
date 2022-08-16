{-# LANGUAGE DuplicateRecordFields #-}

module Spectrum.Executor.EventSource.Data.Tx
  ( MinimalUnconfirmedTx(..)
  , MinimalConfirmedTx(..)
  , MinimalTx(..)
  , fromAlonzoLedgerTx
  ) where

import qualified Ledger as P
import qualified Data.Set as Set

import Data.ByteString.Short (ShortByteString, fromShort)
import qualified Data.Sequence.Strict as StrictSeq

import qualified Cardano.Ledger.Alonzo.Tx as Al

import qualified Cardano.Protocol.TPraos.BHeader as TPraos
import qualified Cardano.Crypto.Hash.Class as CC

import qualified PlutusTx.Prelude as PlutusTx

import CardanoTx.Models
  ( FullTxOut (FullTxOut), TxOutDatum (EmptyDatum, KnownDatumHash) )
import Spectrum.Executor.EventSource.Data.TxContext
  ( TxCtx(MempoolCtx, LedgerCtx) )
import Ouroboros.Consensus.Cardano.Block
    ( AlonzoEra, EraCrypto, StandardCrypto )
import Ouroboros.Consensus.Shelley.Ledger (ShelleyHash (unShelleyHash))
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.AuxiliaryData as Ledger
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Block as Ledger
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Credential as Ledger
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.SafeHash as Ledger
import qualified Cardano.Ledger.TxIn as Ledger
import Cardano.Ledger.Crypto (Crypto)
import Data.ByteString.Short (fromShort)

import qualified Ledger.Tx.CardanoAPI as Interop
import RIO ((<&>))
import Cardano.Api.Shelley (fromShelleyTxIn, fromShelleyTxOut, ShelleyBasedEra (ShelleyBasedEraAlonzo))
import Data.Foldable (Foldable(toList))

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

fromAlonzoLedgerTx
  :: (Crypto crypto, crypto ~ StandardCrypto)
  => ShelleyHash (EraCrypto (AlonzoEra crypto))
  -> Al.ValidatedTx (AlonzoEra crypto) -> MinimalTx 'LedgerCtx
fromAlonzoLedgerTx blockHash vtx =
  let
    body = Al.body vtx
    blockId
      = P.BlockId
      . CC.hashToBytes
      . TPraos.unHashHeader
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
      Interop.fromCardanoTxOut (fromShelleyTxOut ShelleyBasedEraAlonzo tout) <&> (\P.TxOut{..} ->
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
