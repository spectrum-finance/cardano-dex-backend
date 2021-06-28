module Resolver.HttpServer
    ( app
    ) where

import           Control.Monad.IO.Class       (liftIO)
import RIO (Maybe, ($))
import Data.Int
import Dex.Models
import Resolver.PoolsResolver (resolve)
import           Servant
import           Servant.API
import           Network.Wai
import           Network.Wai.Handler.Warp
import RIO.ByteString

type Api =
    "resolve" :> Capture "poolId" PoolId :> Get '[JSON] (Maybe Pool)

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy server

server :: Server Api
server =
    resolvePool

resolvePool :: PoolId -> Handler (Maybe Pool)
resolvePool pId = 
    liftIO $ resolve pId