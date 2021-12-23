module Core.Extractor.AlonzoTxExtractors where

import qualified Cardano.Api       as C
import           Core.Extractor.Class
import           CardanoTx.Models
import           ErgoDex.Amm.Pool
import           RIO

instance Extractor (C.Tx C.AlonzoEra) (FullTxOut, Pool) where
  extract tx = undefined