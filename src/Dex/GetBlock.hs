module Dex.GetBlock 
    (getBlock) where

import Dex.Models.Rosetta.Response.BlockResponse (BlockResponse)
import Dex.Models.AppSettings (HttpSettings(..))
import Dex.Models.Rosetta.Request.BlockRequest  
    ( BlockRequest(..)
    , NetworkIdentifier(..)
    , BlockIdentifier(..)
    )
import qualified Network.HTTP.Simple as HTTP
import RIO
import RIO.Text as T
import Prelude (print)

getBlock :: Int -> RIO HttpSettings BlockResponse 
getBlock height = do
    settings <- ask
    let body = BlockRequest 
            (NetworkIdentifier (blockchainIdentifierS settings) 
            (networkIdentifierS settings)) $ BlockIdentifier height
    let req = HTTP.defaultRequest
            & HTTP.setRequestHost (T.encodeUtf8 $ hostS settings)
            & HTTP.setRequestPort (portS settings)
            & HTTP.setRequestPath "/block"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body
    r <- HTTP.httpJSON req :: RIO HttpSettings (HTTP.Response BlockResponse)
    let response = HTTP.getResponseBody r
    liftIO $ pure response