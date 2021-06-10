{-# LANGUAGE OverloadedStrings #-}

module Dex.HttpClient 
    ( getUnspendOutsStream 
    ) where

import Dex.Models.AppSettings (HttpSettings(..), HasHttpSettings(httpSettingsL))
import RIO ( ($), Either, view, String, RIO )
import Dex.Models.ApiTxOut ( ApiTxOut )
import Conduit ( (.|), runConduit )
import Network.HTTP.Req
    ( (/:),
      defaultHttpConfig,
      http,
      port,
      reqBr,
      runReq,
      GET(GET),
      NoReqBody(NoReqBody) )
import Network.HTTP.Req.Conduit ( responseBodySource )
import Prelude as P (print)
import Data.Default.Class ()
import Data.Conduit.Binary as B ()
import Text.URI ()
import Data.Aeson ( eitherDecode )
import Data.Conduit.Combinators as C ( map, mapM_ )
import RIO.ByteString.Lazy ( fromStrict )
import qualified RIO.ByteString.Lazy as BL
import RIO.Text as T ( pack )

-- ---------- Types declaration ----------

-- ---------- Utils functions ------------

-- ---------- Module api -----------------

getUnspendOutsStream :: HasHttpSettings env => RIO env ()
getUnspendOutsStream = do
    settings <- view httpSettingsL
    runReq defaultHttpConfig $ do
        let url = http (T.pack $ hostS settings) /: "api" /: "v0" /: "tx" /: "outs" /: "unspent"
        reqBr GET url NoReqBody (port $ portS settings) $ \r ->
            runConduit $ 
                responseBodySource r
                    .| C.map fromStrict
                    .| C.map (eitherDecode :: BL.ByteString -> Either String ApiTxOut)
                    .| C.mapM_ P.print