module Dex.HttpClient 
    ( getUnspendOuts 
    ) where

import Dex.Models.AppSettings (HttpSettings(..), HasHttpSettings(httpSettingsL))
import qualified Network.HTTP.Simple as HTTP
import RIO
import RIO.Text as T
import Dex.Models.ApiTxOut

-- ---------- Types declaration ----------

-- ---------- Utils functions ------------

-- ---------- Module api -----------------

-- Use stream api in the future

getUnspendOuts :: HasHttpSettings env => RIO env ApiTxOut
getUnspendOuts = do
    settings <- view httpSettingsL
    let req = HTTP.defaultRequest
            & HTTP.setRequestHost (T.encodeUtf8 $ hostS settings)
            & HTTP.setRequestPort (portS settings)
            & HTTP.setRequestPath "/api/v0/tx/outs/unspent"
            & HTTP.setRequestMethod "GET"
    r <- HTTP.httpJSON req :: RIO env (HTTP.Response ApiTxOut)
    liftIO $ pure $ HTTP.getResponseBody r