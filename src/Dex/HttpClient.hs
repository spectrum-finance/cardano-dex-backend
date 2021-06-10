{-# LANGUAGE OverloadedStrings #-}

module Dex.HttpClient 
    ( getUnspendOutsStream 
    ) where

import Dex.Models.AppSettings (HttpSettings(..), HasHttpSettings(httpSettingsL))
import qualified Network.HTTP.Simple as HTTP
import RIO
import RIO.Text as T
import Dex.Models.ApiTxOut
import Conduit
import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import Network.HTTP.Req ( reqBr, GET(GET), NoReqBody(NoReqBody) )
import Prelude(print)
import Data.Default.Class
import Data.Conduit.Binary as B

-- ---------- Types declaration ----------

-- ---------- Utils functions ------------

-- ---------- Module api -----------------

-- Use stream api in the future

-- getUnspendOuts :: HasHttpSettings env => RIO env ApiTxOut
-- getUnspendOuts = do
--     settings <- view httpSettingsL
--     let req = HTTP.defaultRequest
--             & HTTP.setRequestHost (T.encodeUtf8 $ hostS settings)
--             & HTTP.setRequestPort (portS settings)
--             & HTTP.setRequestPath "/api/v0/tx/outs/unspent"
--             & HTTP.setRequestMethod "GET"
--     r <- HTTP.httpJSON req :: RIO env (HTTP.Response ApiTxOut)
--     liftIO $ pure $ HTTP.getResponseBody r
--  sinkFile :: MonadResource m => FilePath -> ConduitT ByteString o m ()

--  mapM_ :: Monad m => (Word8 -> m ()) -> ConduitT ByteString o m ()

-- test :: ConduitT ByteString o IO ()
-- test = B.mapM_ liftprint


getUnspendOutsStream :: IO ()
getUnspendOutsStream = runReq defaultHttpConfig $ do
    -- settings <- view httpSettingsL
    -- let path = "api" /: "v0" /: "tx" /: "outs" /: "unspent"
    -- (hostS settings ++ ":" ++ hostS settings) /: path
    let url = http "0.0.0.0:8081" /: "api" /: "v0" /: "tx" /: "outs" /: "unspent"
    reqBr GET url NoReqBody mempty $ \r ->
        runConduit $
            (responseBodySource r .| B.mapM_ print)