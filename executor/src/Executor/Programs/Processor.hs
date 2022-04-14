module Executor.Programs.Processor where

import Executor.Services.OrdersExecutor
import Streaming.Consumer
import Streaming.Events
import Streaming.Types
import Debug.Trace

import ErgoDex.Amm.Pool ( PoolId )
import ErgoDex.State 
import ErgoDex.Amm.Orders

import GHC.Exception.Type
import qualified Streamly.Prelude as S
import           RIO
import           Control.Monad.Catch

data Processor f = Processor 
  { run :: f ()
  }

mkProcessor 
  :: (S.MonadAsync f, MonadCatch f)
  => OrdersExecutor f
  -> Consumer f PoolId ConfirmedOrderEvent
  -> Processor f
mkProcessor exec cons = Processor $ run' exec cons

run'
  :: (S.MonadAsync f, MonadCatch f)
  => OrdersExecutor f
  -> Consumer f PoolId ConfirmedOrderEvent
  -> f ()
run' OrdersExecutor{..} Consumer{..} =
    upstream
  & S.mapM (\a -> fmap (\_ -> a) (liftIO $ Debug.Trace.traceM $ "executor run") )
  & S.map mkConfirmedOrder
  & S.mapM process
  & S.handle (\(a :: SomeException) -> (liftIO $ Debug.Trace.traceM $ ("consumer error: " ++ (show a)))) -- log.info here
  & S.drain

mkConfirmedOrder :: (PoolId, ConfirmedOrderEvent) -> Confirmed AnyOrder
mkConfirmedOrder (_, ConfirmedOrderEvent{..}) = Confirmed txOut anyOrder
    