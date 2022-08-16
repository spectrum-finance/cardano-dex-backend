{-# LANGUAGE TypeOperators #-}

module Spectrum.Prelude
  ( UnliftIO
  ) where

import Spectrum.HigherKind (type (~>))

type UnliftIO m = m ~> IO
