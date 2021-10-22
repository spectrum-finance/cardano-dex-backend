module Streaming.Types where

import Data.String

newtype TopicId = TopicId { unTopicId :: String }
  deriving (IsString)
