module Tracker.Models.KafkaModel
  ( KafkaMsg(..)
  ) where

import Cardano.Models
import ErgoDex.Amm.Pool

import Data.Aeson
import GHC.Generics
import RIO.ByteString

data KafkaMsg = KafkaMsg
  { txOut          :: FullTxOut
  , anyOrderPoolId :: PoolId
  , order          :: ByteString
  } deriving (Generic, FromJSON, ToJSON)