module Streaming.Types where

import Data.String
import Control.Exception.Base
import qualified Control.Monad.Error as Err
import Prelude
import Dhall

newtype TopicId = TopicId { unTopicId :: String }
  deriving (Generic, IsString)

instance FromDhall TopicId

data ProducerExecption = ProducerExecption
  deriving Show

instance Exception ProducerExecption

instance Err.Error ProducerExecption

data ConsumerException = ConsumerException
  deriving Show

instance Exception ConsumerException

instance Err.Error ConsumerException