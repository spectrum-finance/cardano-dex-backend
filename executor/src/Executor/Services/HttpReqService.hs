module Executor.Services.HttpReqService 
    ( HttpReqService(..)
    , mkHttpReqService
    ) where

import Executor.Models.Settings
import RIO as R
import Network.HTTP.Req
import RIO.Text as T ( pack )
import Data.List as L ( foldl )
import Dex.Models
import Data.Aeson

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
sendPredicted' settings pool = basePostReq settings ["update"] pool

-- ---------- Experimental feature -------