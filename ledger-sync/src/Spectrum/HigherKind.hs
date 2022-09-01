{-# LANGUAGE TypeOperators #-}

module Spectrum.HigherKind
  ( type (~>)
  , FunctorK(..)
  , LiftK(..)
  ) where

import Data.Kind (Type)
import Control.Monad.Trans.Class (MonadTrans(lift))

type f ~> g = forall a. f a -> g a

class FunctorK (alg :: (Type -> Type) -> Type) where
  fmapK :: (f ~> g) -> alg f -> alg g

class LiftK (f :: Type -> Type) (g :: Type -> Type) where
  liftK :: f ~> g

instance (MonadTrans g, Monad f) => LiftK f (g f) where
  liftK = lift
