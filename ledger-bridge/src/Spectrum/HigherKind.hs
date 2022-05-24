{-# LANGUAGE TypeOperators #-}

module Spectrum.HigherKind
  ( type (~>)
  , FunctorK(..)
  ) where

import Data.Kind (Type)

type f ~> g = forall a. f a -> g a

class FunctorK (alg :: (Type -> Type) -> Type) where
  fmapK :: (f ~> g) -> alg f -> alg g
