{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Executor.Services.OrdersExecutor
  ( OrdersExecutor(..)
  , mkOrdersExecutor
  ) where

import SdkCore.Throw.Combinators
import Data.Either.Combinators
import RIO
import Core.Extractor.AlonzoTxExtractors
import Control.Exception as CE
import Core.Extractor.Class
import ErgoDex.Amm.PoolActions
import Executor.Services.PoolsResolver
import Executor.Models.Errors
import RIO
import Tracker.Services.Logger as Log
import qualified Prelude

import ErgoDex.State 
import ErgoDex.Amm.Pool
import ErgoDex.Amm.Orders
import ErgoDex.Amm.PoolActions
import CardanoTx.Models
import SubmitAPI.Service
import Core.Types

throwEither1 :: (MonadThrow f, Exception e, Show r, MonadIO f, Show e) => Either e r -> f r
throwEither1 (Left err)    = do
  throwM err
throwEither1 (Right value) = do
  pure value

data OrdersExecutor f = OrdersExecutor
  { process :: Confirmed AnyOrder -> f () 
  }

mkOrdersExecutor 
  :: (MonadThrow f, MonadIO f, MonadUnliftIO f, Applicative f)
  => PoolActions f
  -> PoolsResolver f
  -> Transactions f
  -> OrdersExecutor f
mkOrdersExecutor pa pr submitService = OrdersExecutor $ process' pa pr submitService

process' 
  :: forall f. (MonadThrow f, MonadIO f, MonadUnliftIO f, Applicative f)
  => PoolActions f
  -> PoolsResolver f
  -> Transactions f
  -> Confirmed AnyOrder
  -> f ()
process' poolActions PoolsResolver{..} Transactions{..} confirmedOrder@(Confirmed _ (AnyOrder poolId action)) = do
  _ <- Log.log "process1"
  maybePool                      <- resolvePool poolId
  _ <- Log.log ("process2. " ++ (show action))
  pool@(ConfirmedPool confPool)  <- throwMaybe EmptyPoolErr maybePool
  _ <- Log.log ("action " ++ (testShow action))
  _ <- Log.log "process3"
  maybeTx  <- runOrder pool confirmedOrder poolActions
--  let
--    a = RIO.catch (runOrder pool confirmedOrder poolActions) handler
--          where handler :: SomeException -> f (Either OrderExecErr (TxCandidate, Predicted Pool))
--                handler _ = (RIO.pure $ Left PriceTooHigh)
--    b = rightToMaybe $ maybeTx
--  test <- a
--  _    <- case test of
--            Right rig -> Log.log ("a." ++ (show (snd rig)))
--            Left  c -> Log.log ("b." ++ (show (c)))
  -- _ <- Log.log ("test." ++ (show (maybeTx)))
  (txCandidate, predictedPool) <- throwEither1 maybeTx
  _ <- Log.log ("======================================")
  _ <- Log.log ("txCandidate: " ++ (show txCandidate))
  _ <- Log.log ("======================================")
  _ <- Log.log ("predictedPool: " ++ (show predictedPool))
  _ <- Log.log "process5"
  finalTx                        <- finalizeTx txCandidate
  _ <- Log.log "process6"
  let
    mPool = extract finalTx :: Maybe (FullTxOut, Pool)
  _ <- Log.log "process7"
  (out, pp)                      <- throwMaybe EmptyPoolErr mPool
  _ <- Log.log "process8"
  let
    predictedPool = PredictedPool $ OnChainIndexedEntity pp out (getGix confPool)
  _ <- Log.log "process9"
  _                              <- sendPredicted predictedPool
  _ <- Log.log "process10"
  submitTx finalTx

testShow :: OrderAction a -> [Char]
testShow (SwapAction swap) = "swap"
testShow (DepositAction deposit) = "deposit"
testShow (RedeemAction redeem) = "redeem"

runOrder
  :: ConfirmedPool
  -> Confirmed AnyOrder 
  -> PoolActions f
  -> f (Either OrderExecErr (TxCandidate, Predicted Pool))
runOrder (ConfirmedPool (OnChainIndexedEntity pool fullTxOut _)) (Confirmed txOut (AnyOrder _ order)) PoolActions{..} =
  case order of
    DepositAction deposit -> runDeposit (Confirmed txOut deposit) (fullTxOut, pool)
    RedeemAction redeem   -> runRedeem (Confirmed txOut redeem) (fullTxOut, pool)
    SwapAction swap       -> runSwap (Confirmed txOut swap) (fullTxOut, pool)