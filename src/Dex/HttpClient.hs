module Dex.HttpClient 
    ( getUnspentOuts
    , getCurrentHeight
    ) where

import Dex.Models.AppSettings (HttpSettings(..), HasHttpSettings(httpSettingsL))
import RIO as R
import Dex.Models.ApiTxOut ( ApiTxOut )
import Conduit ( (.|), runConduit )
import Network.HTTP.Req
import Network.HTTP.Req.Conduit ( responseBodySource )
import Prelude as P (print)
import Data.Default.Class ()
import Data.Conduit.Binary as B ()
import Data.Aeson ( eitherDecode, FromJSON )
import Text.URI ()
import Data.Conduit.Combinators as C ( map, mapM_ )
import RIO.ByteString.Lazy ( fromStrict )
import qualified RIO.ByteString.Lazy as BL
import RIO.Text as T ( pack )
import Data.List as L ( foldl )

-- ---------- Types declaration ----------

-- ---------- Utils functions ------------

baseGetReq :: forall a env . (FromJSON a, HasHttpSettings env) => [Text] -> RIO env a
baseGetReq reqPaths = do
    settings <- view httpSettingsL
    runReq defaultHttpConfig $ do
        let uri = L.foldl (/:) (http (T.pack $ hostS settings)) reqPaths
        r <- req GET uri NoReqBody jsonResponse (port $ portS settings)
        let result = responseBody r :: a
        pure result

-- ---------- Module api -----------------

-- Get current unspent boxes from the chain --
getUnspentOuts :: HasHttpSettings env => RIO env [ApiTxOut]
getUnspentOuts = baseGetReq ["api", "v0", "tx", "outs", "unspent"]

-- Get current chain height
getCurrentHeight :: HasHttpSettings env => RIO env Int
getCurrentHeight = baseGetReq ["api", "v0", "block", "height"]

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