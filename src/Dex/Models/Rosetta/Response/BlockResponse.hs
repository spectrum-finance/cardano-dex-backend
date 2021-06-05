{-# LANGUAGE DeriveGeneric #-}

module Dex.Models.Rosetta.Response.BlockResponse
    (BlockResponse(..)) where

import GHC.Generics
import Data.Aeson  
import Data.Aeson.Types     
import Data.Aeson.Casing       
import RIO


data BlockIdentifier = BlockIdentifier 
    { hash :: Text
    , index :: Int
    } deriving (Show, Generic, FromJSON)

data Metadata = Metadata 
    { createdBy :: Text
    , epochNo :: Int
    , size :: Int
    , slotNo :: Int
    , transactionsCount :: Int
    } deriving (Show, Generic, FromJSON)

data Block = Block
    { blockIdentifier :: BlockIdentifier
    , parentBlockIdentifier :: BlockIdentifier
    , timestamp :: Int
    , transactions :: [Text]
    , metadata :: Metadata
    } deriving (Show, Generic)

instance FromJSON Block where        
    parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

-- todo: add failed response model
data BlockResponse = BlockResponse
    { block :: Block } deriving (Show, Generic, FromJSON)