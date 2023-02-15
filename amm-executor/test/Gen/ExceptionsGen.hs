module Gen.ExceptionsGen where

import Control.Exception (Exception)

data BadInputsUTxOException = BadInputsUTxOException

instance Show BadInputsUTxOException where
  show _ = "BadInputsUTxO"

instance Exception BadInputsUTxOException
