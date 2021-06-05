module Dex.GetBlock 
    (getBlock) where

import Dex.Models.Rosetta.Response.BlockResponse (BlockResponse)
import Dex.Models.Rosetta.Request.BlockRequest  (BlockRequest(..), NetworkIdentifier(..), BlockIdentifier(..))
import qualified Network.HTTP.Simple as HTTP
import Control.Monad.IO.Class
import RIO
import RIO.Text as T

getBlock :: Int -> IO BlockResponse 
getBlock height = do
    let body = BlockRequest 
            (NetworkIdentifier (T.pack "cardano") 
            (T.pack "testnet")) $ BlockIdentifier height
    let req = HTTP.defaultRequest
            & HTTP.setRequestHost "0.0.0.0"
            & HTTP.setRequestPort 8080
            & HTTP.setRequestPath "/block"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body
    r <- HTTP.httpJSON req :: IO (HTTP.Response BlockResponse)
    let response = HTTP.getResponseBody r
    liftIO $ pure response
