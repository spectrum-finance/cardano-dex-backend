module Spectrum.Executor.OrdersExecutor.RefInputs where

import CardanoTx.Models (FullTxOut)
import Spectrum.Executor.Config (TxRefs(..))
import RIO
import Explorer.Service (Explorer(..))
import qualified Ledger.Tx.CardanoAPI as Interop
import Explorer.Class (ToCardanoTx(toCardanoTx))

data RefInputs = RefInputs
  { swapOutput    :: FullTxOut
  , redeemOutput  :: FullTxOut
  , depositOutput :: FullTxOut
  , poolOutputV1  :: FullTxOut
  , poolOutputV2  :: FullTxOut
  }

data CouldnotRetriveRefInputs = CouldnotRetriveRefInputs deriving Show
instance Exception CouldnotRetriveRefInputs

mkRefInputs :: (MonadIO m, MonadThrow m) => TxRefs -> Explorer m -> m RefInputs
mkRefInputs TxRefs{..} Explorer{..} = do
  let
    poolOutV1Ref = Interop.fromCardanoTxIn poolV1Ref
    poolOutV2Ref = Interop.fromCardanoTxIn poolV2Ref
    depositOutRef = Interop.fromCardanoTxIn depositRef
    redeemOutRef = Interop.fromCardanoTxIn redeemRef
    swapOutRef = Interop.fromCardanoTxIn swapRef
  poolV1RefOuput  <- getOutput poolOutV1Ref
  poolV2RefOutput <- getOutput poolOutV2Ref
  depositRefOut <- getOutput depositOutRef
  redeemRefOut  <- getOutput redeemOutRef
  swapRefOut    <- getOutput swapOutRef
  case catMaybes [swapRefOut, redeemRefOut, depositRefOut, poolV1RefOuput, poolV2RefOutput] <&> toCardanoTx of
      [swap, redeem, deposit, poolV1, poolV2] ->
          pure $ RefInputs swap redeem deposit poolV1 poolV2
      _ -> throwM CouldnotRetriveRefInputs