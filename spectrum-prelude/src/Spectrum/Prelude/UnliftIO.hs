{-# LANGUAGE TypeOperators #-}

module Spectrum.Prelude.UnliftIO
  ( UnliftIO
  ) where

import Spectrum.HigherKind (type (~>))

type UnliftIO m = m ~> IO
