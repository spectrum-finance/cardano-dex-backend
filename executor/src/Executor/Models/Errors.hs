module Executor.Models.Errors where

import RIO

data EmptyPoolErr = EmptyPoolErr
  deriving (Show)

instance Exception EmptyPoolErr