module Resolver.HttpServer
    ( runHttpServer
    ) where

import Resolver.Models.AppSettings
import Control.Monad.IO.Class as CIO (liftIO)
import RIO as RIO (Maybe, ($), (>>), RIO(..), view, liftIO)
import Data.Int
import Dex.Models
import Resolver.PoolsResolver (resolve)
import Servant
import Servant.API
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import RIO.ByteString
import Prelude (print)
import Resolver.Models.CfmmPool
import Resolver.Pool

runHttpServer :: HasHttpServerSettings env => RIO env ()
runHttpServer = do
    settings <- view httpSettingsL
    RIO.liftIO $ print "Running http server" >> (Warp.run (port settings) app)

type Api =
         "resolve" :> ReqBody '[JSON] PoolId :> Get '[JSON] (Maybe Pool)
    :<|> "pull"    :> ReqBody '[JSON] Pool :> Post '[JSON] ()

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy server

server :: Server Api
server =
    resolvePool :<|>
    pull
 
resolvePool :: PoolId -> Handler (Maybe Pool)
resolvePool pId = 
    CIO.liftIO $ (print "Going to resolve pool") >> resolve pId

pull :: Pool -> Handler ()
pull pool =
    CIO.liftIO $ (print "Going to pull predicted") >> putPredicted $ PredictedPool pool