module Core.Extractor.Class where

import Data.Maybe
import CardanoTx.Models

class Extractor a b where
  extract :: a -> Maybe b