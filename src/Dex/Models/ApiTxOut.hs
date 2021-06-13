module Dex.Models.ApiTxOut
    ( ApiTxOut(..)
    , ApiAddr(..)
    , ApiContents(..)
    , ApiValue(..)
    , ApiCurrencySymbol(..)
    , ApiTokenName(..)
    ) where

import RIO.Text ( Text )
import RIO ( Show, Generic, Int, Maybe )
import Data.Aeson ( FromJSON )
import GHC.Generics ()

data ApiTxOut = ApiTxOut
    { txOutAddress :: ApiAddr
    , txOutValue :: ApiValue
    , txOutDatumHash :: Maybe Text
    } deriving (Generic, FromJSON, Show)

data ApiAddr = ApiAddr
    { addressStakingCredential :: Maybe Text
    , contents :: ApiContents
    } deriving (Generic, FromJSON, Show)

newtype ApiContents = ApiContents
    { getPubKeyHash :: Text 
    } deriving (Generic, FromJSON, Show)

newtype ApiValue = ApiValue
    { getValue :: [(ApiCurrencySymbol, [(ApiTokenName, Int)])] 
    } deriving (Generic, FromJSON, Show)

newtype ApiCurrencySymbol = ApiCurrencySymbol 
    { unCurrencySymbol :: Text 
    } deriving (Generic, FromJSON, Show)

newtype ApiTokenName = ApiTokenName
    { unTokenName :: Text 
    } deriving (Generic, FromJSON, Show)