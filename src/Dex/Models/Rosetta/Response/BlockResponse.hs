{-# LANGUAGE DeriveGeneric #-}

module Dex.Models.Rosetta.Response.BlockResponse
    (BlockResponse(..)) where

import GHC.Generics
import qualified Data.Text as T
import           Data.Aeson            (FromJSON, ToJSON)

data BlockIdentifier = BlockIdentifier {
    hash :: T.Text,
    index :: Int
} deriving (Show, Generic)

instance ToJSON BlockIdentifier where

instance FromJSON BlockIdentifier where

data Metadata = Metadata {
    createdBy :: T.Text,
    epochNo :: Int,
    size :: Int,
    slotNo :: Int,
    transactionsCount :: Int
}  deriving (Show, Generic)

instance ToJSON Metadata where

instance FromJSON Metadata where

data BlockResponse = BlockResponse {
    blockIdentifier :: BlockIdentifier,
    metadata :: Metadata,
    parantBlockIdentifier :: BlockIdentifier,
    timestamp :: Int,
    transactions :: [T.Text]
} deriving (Show, Generic)

instance ToJSON BlockResponse where

instance FromJSON BlockResponse where