module Executor.Models.Errors where

import RIO

data MaybeErr = MaybeErr
  deriving Show

instance Exception MaybeErr