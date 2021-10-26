module Tracker.Repository.ExplorerRepo where

import Prelude
import Explorer.Types

data ExplorerRepo f = ExplorerRepo
  { putMinIndex :: Gix -> f ()
  , getMinIndex :: f Gix
  }

mkExplorerRepo :: forall f . ExplorerRepo f
mkExplorerRepo = undefined 