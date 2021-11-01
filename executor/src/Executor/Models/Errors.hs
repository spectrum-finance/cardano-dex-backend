module Executor.Models.Errors where

import RIO

data MaybeExecption = MaybeExecption
  deriving Show

instance Exception MaybeExecption