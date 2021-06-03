{-# LANGUAGE DeriveGeneric #-}

module Dex.Models.Rosetta.Request.BlockRequest
    ( BlockRequest(..),
      NetworkIdentifier(..),
      BlockIdentifier(..)
    ) where

import GHC.Generics
import qualified Data.Text as T
import           Data.Aeson            (FromJSON, ToJSON)

data NetworkIdentifier = NetworkIdentifier { 
    blockchain :: T.Text,
    network :: T.Text
} deriving (Generic, Show)

instance ToJSON NetworkIdentifier where

instance FromJSON NetworkIdentifier where

newtype BlockIdentifier = BlockIdentifier {
    index :: Int
} deriving (Show, Generic)

instance ToJSON BlockIdentifier where

instance FromJSON BlockIdentifier where

data BlockRequest = BlockRequest {
    networkIdentifier :: NetworkIdentifier,
    blockIdentifier :: BlockIdentifier
} deriving (Show, Generic)

instance ToJSON BlockRequest where

instance FromJSON BlockRequest where