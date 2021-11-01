module Streaming.Types where

import Data.String
import Control.Exception.Base
import Prelude (Show)
import qualified Control.Monad.Error as Err
import Kafka.Producer
import Prelude
import Control.Monad.Trans.Identity as Identity

newtype TopicId = TopicId { unTopicId :: String }
  deriving (IsString)

data ProducerExecption = ProducerExecption
  deriving Show

instance Exception ProducerExecption

instance Err.Error ProducerExecption
