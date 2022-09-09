module Spectrum.Prelude.Context
  ( HasType
  , MonadReader
  , askContext
  , ask
  ) where

import RIO ( MonadReader(ask), asks )

import Data.Generics.Product.Typed
  ( HasType, typed )
import Data.Generics.Internal.VL.Lens
  ( view )

askContext :: forall m env a. (HasType a env, MonadReader env m) => m a
askContext = asks (view typed)
