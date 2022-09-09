{-# LANGUAGE TypeOperators #-}

module Spectrum.Prelude.UnliftIO
  ( UnliftIO
  ) where

import RIO
  ( IO )
import Spectrum.Prelude.HigherKind
  ( type (~>) )

type UnliftIO m = m ~> IO
