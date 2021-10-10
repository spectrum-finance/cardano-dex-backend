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
import Utils (PoolId(..))
import GHC.Natural

data HttpReqService = HttpReqService
    { resolvePoolReq :: PoolId -> IO (Maybe Pool)
    , sendPredicted :: Pool -> IO ()
    }

mkHttpReqService :: HasSettings env => RIO env HttpReqService
mkHttpReqService = do
    settings <- view httpSettingsL
    pure $ HttpReqService (resolvePoolReq' settings) (sendPredictedReq' settings)
-- ---------- Types declaration ----------
-- ---------- Module api -----------------

resolvePoolReq' :: HttpSettings -> PoolId -> IO (Maybe Pool)
resolvePoolReq' settings poolId = do
    runReq defaultHttpConfig $ do
        let url = L.foldl (/:) (http (T.pack $ hostS settings)) ["resolve"]
        res <- req POST url (ReqBodyJson poolId) jsonResponse (port $ fromIntegral . naturalToInteger $ portS settings)
        pure $ ((responseBody res) :: Maybe Pool)

-- Get current chain height
sendPredictedReq' :: HttpSettings -> Pool -> IO ()
sendPredictedReq' settings pool = do
    runReq defaultHttpConfig $ do
        let uri = L.foldl (/:) (http (T.pack $ hostS settings)) ["update"]
        void $ req POST uri (ReqBodyJson pool) ignoreResponse (port $ fromIntegral . naturalToInteger $ portS settings)

-- ---------- Experimental feature -------