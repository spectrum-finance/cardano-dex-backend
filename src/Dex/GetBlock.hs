module Dex.GetBlock 
    (getBlock) where

import Dex.Models.Rosetta.Response.BlockResponse (BlockResponse)
import Dex.Models.Rosetta.Request.BlockRequest  (BlockRequest(..), NetworkIdentifier(..), BlockIdentifier(..))
import Network.HTTP.Req
import qualified Data.Text as T
import Control.Monad.IO.Class

getBlock :: Int -> IO BlockResponse 
getBlock height = runReq defaultHttpConfig $ do
    let body = BlockRequest 
            (NetworkIdentifier (T.pack "cardano") 
            (T.pack "testnet")) $ BlockIdentifier height
    r <- req 
            POST 
            (https "localhost:8080" /: "block") 
            (ReqBodyJson body) 
            jsonResponse 
            mempty
    let response = responseBody r :: BlockResponse
    liftIO $ pure response