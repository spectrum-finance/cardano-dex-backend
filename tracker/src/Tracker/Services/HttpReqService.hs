module Tracker.Services.HttpReqService 
    ( HttpReqService(..)
    , mkHttpReqService
    ) where

import Tracker.Models.AppSettings (HttpSettings(..), HasHttpSettings(httpSettingsL))
import RIO as R
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
import Plutus.V1.Ledger.Tx ( TxOut(..) )
import Dex.Models

data HttpReqService env = HttpReqService
    { getUnspentOuts :: HasHttpSettings env => RIO env [FullTxOut]
    , getCurrentHeight :: HasHttpSettings env => RIO env Int
    }

mkHttpReqService :: HttpReqService env
mkHttpReqService = HttpReqService getUnspentOuts' getCurrentHeight'

getUnspentOuts' :: HasHttpSettings env => RIO env [FullTxOut]
getUnspentOuts' = baseGetReq ["api", "v0", "tx", "outs", "unspent"]

getCurrentHeight' :: HasHttpSettings env => RIO env Int
getCurrentHeight' = baseGetReq ["api", "v0", "block", "height"]

-------------------------------------------------------------------------------------

baseGetReq :: forall a env . (FromJSON a, HasHttpSettings env) => [Text] -> RIO env a
baseGetReq reqPaths = do
    settings <- view httpSettingsL
    runReq defaultHttpConfig $ do
        let uri = L.foldl (/:) (http (T.pack $ getHost settings)) reqPaths
        r <- req GET uri NoReqBody jsonResponse (port $ getPort settings)
        let result = responseBody r :: a
        pure result

-- ---------- Experimental feature -------

getUnspentOutsStream :: HasHttpSettings env => RIO env ()
getUnspentOutsStream = do
    settings <- view httpSettingsL
    runReq defaultHttpConfig $ do
        let url = http (T.pack $ getHost settings) /: "experimental" /: "api" /: "v0" /: "tx" /: "outs" /: "unspent"
        reqBr GET url NoReqBody (port $ getPort settings) $ \r ->
            runConduit $ 
                responseBodySource r
                    .| C.map fromStrict
                    .| C.map (eitherDecode :: BL.ByteString -> Either String TxOut)
                    .| C.mapM_ P.print