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
  , poolOutput    :: FullTxOut
  }

data CouldnotRetriveRefInputs = CouldnotRetriveRefInputs deriving Show
instance Exception CouldnotRetriveRefInputs

mkRefInputs :: (MonadIO m, MonadThrow m) => TxRefs -> Explorer m -> m RefInputs
mkRefInputs TxRefs{..} Explorer{..} = do
  let
    poolOutRef = Interop.fromCardanoTxIn poolV1Ref
    depositOutRef = Interop.fromCardanoTxIn depositRef
    redeemOutRef = Interop.fromCardanoTxIn redeemRef
    swapOutRef = Interop.fromCardanoTxIn swapRef
  poolV1RefOuput  <- getOutput poolOutRef
  depositRefOut <- getOutput depositOutRef
  redeemRefOut  <- getOutput redeemOutRef
  swapRefOut    <- getOutput swapOutRef
  case catMaybes [swapRefOut, redeemRefOut, depositRefOut, poolV1RefOuput] <&> toCardanoTx of
      [swap, redeem, deposit, pool] ->
          pure $ RefInputs swap redeem deposit pool
      _ -> throwM CouldnotRetriveRefInputs