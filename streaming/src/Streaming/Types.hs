module Streaming.Types where

import Data.String
import Control.Exception.Base
import Prelude (Show)
import Control.Monad.Error

newtype TopicId = TopicId { unTopicId :: String }
  deriving (IsString)

data ProducerExecption = ProducerExecption
  deriving Show

instance Exception ProducerExecption

instance Error ProducerExecption