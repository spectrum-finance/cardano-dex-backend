module Tracker.Utils 
    ( unsafeFromEither,
      toFullTxOut
    ) where

import RIO
import Tracker.Models.ExplorerModels
import Dex.Models

unsafeFromEither :: Either String a -> a
unsafeFromEither (Left err)    = error err
unsafeFromEither (Right value) = value

toFullTxOut :: ApiFullTxOut -> FullTxOut
toFullTxOut _ = undefined
