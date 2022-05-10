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

data Resolver f = Resolver
  { run :: f ()
  }

mkResolver
  :: (S.MonadAsync f, MonadCatch f)
  => PoolRepository f
  -> Consumer f PoolId ConfirmedPoolEvent
  -> Resolver f
mkResolver repo cons = Resolver $ run' repo cons

run'
  :: (S.MonadAsync f, MonadCatch f)
  => PoolRepository f
  -> Consumer f PoolId ConfirmedPoolEvent
  -> f ()
run' PoolRepository{..} Consumer{..} =
    upstream
  & S.map (\(_, ConfirmedPoolEvent{..}) -> ConfirmedPool (OnChainIndexedEntity pool txOut gix))
  & S.mapM putConfirmed
  & S.handle (\ConsumerException -> S.fromPure ())
  & S.drain