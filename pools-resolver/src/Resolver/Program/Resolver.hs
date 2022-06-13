module Resolver.Program.Resolver
  ( Resolver(..)
  , mkResolver
  ) where

import RIO
import Control.Monad.Catch

import ErgoDex.Amm.Pool
import Core.Types
import Resolver.Repositories.PoolRepository

import qualified Streamly.Prelude as S
import           Streaming.Types
import           Streaming.Events
import           Streaming.Consumer
import System.Logging.Hlog

data Resolver f = Resolver
  { run :: f ()
  }

mkResolver
  :: (Monad i, S.MonadAsync f, MonadCatch f)
  => PoolRepository f
  -> MakeLogging i f
  -> Consumer f PoolId ConfirmedPoolEvent
  -> i (Resolver f)
mkResolver repo MakeLogging{..} cons = do
  logging <- forComponent "Resolver"
  pure $ Resolver $ run' repo logging cons

run'
  :: (S.MonadAsync f, MonadCatch f)
  => PoolRepository f
  -> Logging f
  -> Consumer f PoolId ConfirmedPoolEvent
  -> f ()
run' PoolRepository{..} Logging{..} Consumer{..} =
    upstream
  & S.mapM (\a -> fmap (const a) (infoM @String "Going to process msgs") )
  & S.map mkConfirmedPool
  & S.mapM putConfirmed
  & S.handle (\(a :: SomeException) -> lift . errorM $ ("resolver error: " ++ show a))
  & S.drain

mkConfirmedPool :: (PoolId, ConfirmedPoolEvent) -> ConfirmedPool
mkConfirmedPool (_, ConfirmedPoolEvent{..}) = ConfirmedPool (OnChainIndexedEntity pool txOut gix)