module Resolver.HttpServer
    ( app
    ) where

import           Control.Monad.IO.Class       (liftIO)
import RIO (Maybe, ($), (>>))
import Data.Int
import Dex.Models
import Resolver.PoolsResolver (resolve)
import           Servant
import           Servant.API
import           Network.Wai
import           Network.Wai.Handler.Warp
import RIO.ByteString
import Prelude (print)
import Resolver.Models.CfmmPool
import Resolver.Pool

type Api =
         "resolve" :> ReqBody '[JSON] PoolId :> Get '[JSON] (Maybe Pool)
    :<|> "check"   :> Get '[JSON] ()
    :<|> "pull"    :> ReqBody '[JSON] Pool :> Post '[JSON] ()

apiProxy :: Proxy Api
apiProxy = Proxy

app :: Application
app = serve apiProxy server

server :: Server Api
server =
    resolvePool :<|>
    check :<|>
    pull
 
resolvePool :: PoolId -> Handler (Maybe Pool)
resolvePool pId = 
    liftIO $ (print "Going to resolve pool") >> resolve pId

check :: Handler ()
check = liftIO $ print "check func"

pull :: Pool -> Handler ()
pull pool =
    liftIO $ putPredicted $ PredictedPool pool