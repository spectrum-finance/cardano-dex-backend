module Resolver.HttpServer
    ( HttpServerService(..)
    , mkHttpServerService
    ) where

import Resolver.Models.AppSettings
import Control.Monad.IO.Class as CIO (liftIO)
import RIO as RIO (Maybe, ($), (>>), RIO(..), view, liftIO)
import Data.Int
import Dex.Models
import Resolver.PoolsResolver (PoolResolver(..))
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import RIO.ByteString
import Prelude (print)
import Resolver.Models.CfmmPool
import Resolver.Pool (PoolApi(..))

data HttpServerService env = HttpServerService
    { runHttpServer :: HasHttpServerSettings env => RIO env () 
    }

mkHttpServerService :: PoolResolver -> PoolApi -> HttpServerService env
mkHttpServerService r p = HttpServerService $ runHttpServer' r p

runHttpServer' :: HasHttpServerSettings env => PoolResolver -> PoolApi -> RIO env ()
runHttpServer' r p = do
    settings <- view httpSettingsL
    RIO.liftIO $ print "Running http server" >> (Warp.run (port settings) (app r p))

type Api =
         "resolve" :> ReqBody '[JSON] PoolId :> Get '[JSON] (Maybe Pool)
    :<|> "pull"    :> ReqBody '[JSON] Pool :> Post '[JSON] ()

apiProxy :: Proxy Api
apiProxy = Proxy

app :: PoolResolver -> PoolApi -> Application
app r p = serve apiProxy (server r p)

server :: PoolResolver -> PoolApi -> Server Api
server r p =
    resolvePool r :<|>
    pull p
 
resolvePool :: PoolResolver -> PoolId -> Handler (Maybe Pool)
resolvePool PoolResolver{..} pId = 
    CIO.liftIO $ (print "Going to resolve pool") >> resolve pId

pull :: PoolApi -> Pool -> Handler ()
pull PoolApi{..} pool =
    CIO.liftIO $ putPredicted $ PredictedPool pool