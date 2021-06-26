module Resolver.HttpServer where

import RIO
import Dex.Models
import Resolver.PoolsResolver

type Api =
    "resolve" :> Capture "poolId" Int :> Get '[JSON] [Pool]

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxyy server

server :: Server Api
server =

resolvePool :: PoolId -> Handler (Maybe Pool)
resolvePool = 
    liftIO $ 