module Spectrum.Context
  ( HasType
  , MonadReader
  , askContext
  ) where

import RIO ( MonadReader, asks )

import Data.Generics.Product.Typed
  ( HasType, typed )
import Data.Generics.Internal.VL.Lens
  ( view )

askContext :: forall m env a. (HasType a env, MonadReader env m) => m a
askContext = asks (view typed)
