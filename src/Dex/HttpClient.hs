module Dex.HttpClient 
    ( getUnspentOuts
    , getCurrentHeight
    ) where

import Dex.Models.AppSettings (HttpSettings(..), HasHttpSettings(httpSettingsL))
import RIO
import Dex.Models.ApiTxOut ( ApiTxOut )
import Conduit ( (.|), runConduit )
import Network.HTTP.Req
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

-- Get current unspent boxes from the chain --
getUnspentOuts :: HasHttpSettings env => RIO env [ApiTxOut]
getUnspentOuts = do
    settings <- view httpSettingsL
    runReq defaultHttpConfig $ do
        let url = http (T.pack $ hostS settings) /: "api" /: "v0" /: "tx" /: "outs" /: "unspent"
        r <- req GET url NoReqBody jsonResponse (port $ portS settings)
        let body = responseBody r :: [ApiTxOut]
        pure body

-- Get current chain height
getCurrentHeight :: HasHttpSettings env => RIO env Int
getCurrentHeight = do
    settings <- view httpSettingsL
    runReq defaultHttpConfig $ do
        let url = http (T.pack $ hostS settings) /: "api" /: "v0" /: "block" /: "height"
        r <- req GET url NoReqBody jsonResponse (port $ portS settings)
        let body = responseBody r :: Int
        pure body

-- ---------- Experimental feature -------

-- Stream current unspent boxes from the chain --
getUnspentOutsStream :: HasHttpSettings env => RIO env ()
getUnspentOutsStream = do
    settings <- view httpSettingsL
    runReq defaultHttpConfig $ do
        let url = http (T.pack $ hostS settings) /: "experimental" /: "api" /: "v0" /: "tx" /: "outs" /: "unspent"
        reqBr GET url NoReqBody (port $ portS settings) $ \r ->
            runConduit $ 
                responseBodySource r
                    .| C.map fromStrict
                    .| C.map (eitherDecode :: BL.ByteString -> Either String ApiTxOut)
                    .| C.mapM_ P.print