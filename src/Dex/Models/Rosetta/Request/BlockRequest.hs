{-# LANGUAGE DeriveGeneric #-}

module Dex.Models.Rosetta.Request.BlockRequest
    ( BlockRequest(..),
      NetworkIdentifier(..),
      BlockIdentifier(..)
    ) where

import GHC.Generics
import Data.Aeson 
import RIO
import Data.Aeson.Casing  

data NetworkIdentifier = NetworkIdentifier 
    { blockchain :: Text
    , network :: Text
    } deriving (Generic, Show, ToJSON)

newtype BlockIdentifier = BlockIdentifier 
    { index :: Int } deriving (Show, Generic, ToJSON)

data BlockRequest = BlockRequest 
    { networkIdentifier :: NetworkIdentifier
    , blockIdentifier :: BlockIdentifier 
    } deriving (Show, Generic)

instance ToJSON BlockRequest where
    toJSON = genericToJSON $ aesonDrop 0 snakeCase