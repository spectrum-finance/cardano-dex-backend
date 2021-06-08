{-# LANGUAGE DeriveGeneric #-}

module Dex.Models.Rosetta.Response.BlockResponse
    (BlockResponse(..)) where

import GHC.Generics
import Data.Aeson  
import Data.Aeson.Types     
import Data.Aeson.Casing       
import RIO
import RIO.Text
import RIO.Char as C


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
    , transactions :: [Transaction]
    , metadata :: Metadata
    } deriving (Show, Generic)

instance FromJSON Block where        
    parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

-- todo: add failed response model
data BlockResponse = BlockResponse
    { block :: Block } deriving (Show, Generic, FromJSON)

data Transaction = Transaction
    { transactionIdentifier :: TransactionIdentifier
    , operations :: [TransactionOperation]
    } deriving (Show, Generic)

instance FromJSON Transaction where        
    parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

data TransactionIdentifier = TransactionIdentifier
    { txnHash :: Text } deriving (Show, Generic)

instance FromJSON TransactionIdentifier where        
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data IntputOperationIdentifier = IntputOperationIdentifier
    { inputIndex :: Int } deriving (Show, Generic)

instance FromJSON IntputOperationIdentifier where        
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Account = Account
    { address :: Text } deriving (Show, Generic, FromJSON)

data Currency = Currency
    { symbol :: Text
    , decimals :: Int 
    } deriving (Show, Generic, FromJSON)

data Amount = Amount
    { value :: Text
    , currency :: Currency 
    } deriving (Show, Generic, FromJSON)

data CoinIdentifier = CoinIdentifier
    { identifier :: Text }  deriving (Show, Generic)

instance FromJSON CoinIdentifier where        
    parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

data CoinChange = CoinChange
    { coinIdentifier :: CoinIdentifier
    , coinAction :: Text
    } deriving (Show, Generic)

instance FromJSON CoinChange where        
    parseJSON = genericParseJSON $ aesonDrop 0 snakeCase

data OutputOperationIdentifier = OutputOperationIdentifier
    { outputIndex :: Int
    , outputNetworkIndex :: Int
    } deriving (Show, Generic)

instance FromJSON OutputOperationIdentifier where        
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data OutputRelatedOperations = OutputRelatedOperations
    { opIndex :: Int } deriving (Show, Generic)

instance FromJSON OutputRelatedOperations where        
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data TransactionOperation = 
    Input
        { intputOperationIdentifier :: IntputOperationIdentifier
        , inputStatus :: Text
        , inputAccount :: Account
        , inputAmount :: Amount
        , inputCoinChange :: CoinChange
        } | 
    Output
        { outputCoinChange :: CoinChange
        , outputAmount :: Amount
        , outputAccount :: Account
        , outputStatus :: Text
        , outputOperationIdentifier :: OutputOperationIdentifier
        , outputRelatedOperations :: [OutputRelatedOperations]
        } deriving (Show, Generic)

instance FromJSON TransactionOperation where        
    parseJSON = genericParseJSON $ 
        (aesonPrefix snakeCase) 
            { sumEncoding = TaggedObject { tagFieldName = "type" } 
            , constructorTagModifier = fmap C.toLower
            }