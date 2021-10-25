module Tracker.Clients.ExplorerClient where

-- import Tracker.Models.ExplorerModels
import Tracker.Models.AppSettings
import Prelude
import RIO
import Network.HTTP.Simple
import Data.ByteString.Char8 as Data
import GHC.Natural as Natural
import Explorer.Models

data ExplorerClient = ExplorerClient 
 { getUspentOutputs :: Int -> Int -> IO [FullTxOut]
 }

mkExplorerClient :: ClientSettings -> IO ExplorerClient
mkExplorerClient settings = pure $ ExplorerClient $ getUspentOutputs' settings

getUspentOutputs' :: ClientSettings -> Int -> Int -> IO [FullTxOut]
getUspentOutputs' ClientSettings{..} minIndex limit = undefined -- do
  -- let request = defaultRequest 
  --       & setRequestPath (Data.pack $ "/outputs/unspent/indexed?minIndex=" ++ show minIndex ++ "&limit=" ++ show limit)
  -- 	    & setRequestHost (Data.pack getExplorerHost)
  -- 	    & setRequestPort (Natural.naturalToInt getExplorerPort)
  -- response <- httpJSON request
  -- let result = getResponseBody response :: Items
  -- print $ "ExplorerClient::unspentResultIs=" ++ show result
  -- pure $ items result 
