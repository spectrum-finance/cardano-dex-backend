module Executor.Services.HttpReqService 
    ( HttpReqService(..)
    , mkHttpReqService
    ) where

import Executor.Models.Settings
import RIO as R
import Conduit ( (.|), runConduit )
import Network.HTTP.Req
import Network.HTTP.Req.Conduit ( responseBodySource )
import Prelude as P (print)
import Data.Default.Class ()
import Data.Conduit.Binary as B ()
import Data.Aeson ( eitherDecode, FromJSON,  ToJSON)
import Text.URI ()
import Data.Conduit.Combinators as C ( map, mapM_ )
import RIO.ByteString.Lazy ( fromStrict )
import qualified RIO.ByteString.Lazy as BL
import RIO.Text as T ( pack )
import Data.List as L ( foldl )
import Plutus.V1.Ledger.Tx ( TxOut(..) )
import Dex.Models

data HttpReqService = HttpReqService
    { resolvePoolReq :: IO (Maybe Pool)
    , sendPredicted :: Pool -> IO ()
    }

mkHttpReqService :: HasHttpSettings env => RIO env HttpReqService
mkHttpReqService = do
    settings <- view httpSettingsL
    pure $ HttpReqService (resolvePoolReq' settings) (sendPredicted' settings)
-- ---------- Types declaration ----------

-- ---------- Utils functions ------------

baseGetReq :: forall a . FromJSON a => HttpSettings -> [Text] -> IO a
baseGetReq settings reqPaths = do
    runReq defaultHttpConfig $ do
        let uri = L.foldl (/:) (http (T.pack $ hostS settings)) reqPaths
        r <- req GET uri NoReqBody jsonResponse (port $ portS settings)
        let result = responseBody r :: a
        pure result

basePostReq :: forall b . ToJSON b => HttpSettings -> [Text] -> b -> IO ()
basePostReq settings reqPaths model = do
    runReq defaultHttpConfig $ do
        let uri = L.foldl (/:) (http (T.pack $ hostS settings)) reqPaths
        _ <- req POST uri (ReqBodyJson model) ignoreResponse (port $ portS settings)
        pure ()
-- ---------- Module api -----------------

resolvePoolReq' :: HttpSettings -> IO (Maybe Pool)
resolvePoolReq' settings = baseGetReq settings ["resolve"]

-- Get current chain height
sendPredicted' :: HttpSettings -> Pool -> IO ()
sendPredicted' settings pool = basePostReq settings ["pull"] pool

-- ---------- Experimental feature -------