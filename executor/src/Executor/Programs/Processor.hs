{-# LANGUAGE OverloadedStrings #-}

module Executor.Programs.Processor where

import Executor.Services.OrdersExecutor
import Streaming.Consumer
import Streaming.Events

import ErgoDex.Amm.Pool    ( PoolId )
import ErgoDex.State
import ErgoDex.Amm.Orders
import System.Logging.Hlog

import GHC.Exception.Type
import qualified Streamly.Prelude as S
import           RIO
import           Control.Monad.Catch

data Processor f = Processor
  { run :: f ()
  }

mkProcessor
  :: (Monad i, S.MonadAsync f, MonadCatch f)
  => OrdersExecutor f
  -> MakeLogging i f
  -> Consumer f PoolId ConfirmedOrderEvent
  -> i (Processor f)
mkProcessor exec MakeLogging{..} cons = do
  logger <- forComponent "processor"
  pure $ Processor $ run' exec logger cons

run'
  :: (S.MonadAsync f, MonadCatch f)
  => OrdersExecutor f
  -> Logging f
  -> Consumer f PoolId ConfirmedOrderEvent
  -> f ()
run' OrdersExecutor{..} Logging{..} Consumer{..} =
    upstream
  & S.mapM (\a -> fmap (const a) (infoM @String "Going to process msgs") )
  & S.map mkConfirmedOrder
  & S.mapM process
  & S.handle (\(a :: SomeException) -> lift . errorM $ ("consumer error: " ++ show a)) -- log.info here
  & S.drain

mkConfirmedOrder :: (PoolId, ConfirmedOrderEvent) -> Confirmed AnyOrder
mkConfirmedOrder (_, ConfirmedOrderEvent{..}) = Confirmed txOut anyOrder
    