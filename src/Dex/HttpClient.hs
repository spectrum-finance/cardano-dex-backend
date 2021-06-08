module Dex.HttpClient 
    ( getUnspendOuts 
    ) where

import Dex.Models.AppSettings (HttpSettings(..), HasHttpSettings(httpSettingsL))
import qualified Network.HTTP.Simple as HTTP
import RIO
import RIO.Text as T
import Plutus.V1.Ledger.Tx (TxOut(..))

-- ---------- Types declaration ----------

-- ---------- Utils functions ------------

-- ---------- Module api -----------------

-- Use stream api in the future
getUnspendOuts :: HasHttpSettings env => RIO env TxOut
getUnspendOuts = do
    settings <- view httpSettingsL
    let req = HTTP.defaultRequest
            & HTTP.setRequestHost (T.encodeUtf8 $ hostS settings)
            & HTTP.setRequestPort (portS settings)
            & HTTP.setRequestPath "/api/v0/tx/outs/unspent"
            & HTTP.setRequestMethod "GET"
    r <- HTTP.httpJSON req :: RIO env (HTTP.Response TxOut)
    let response = HTTP.getResponseBody r
    liftIO $ pure response