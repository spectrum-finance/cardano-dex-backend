{-# LANGUAGE TypeOperators #-}

module Resolver.Endpoints.HttpServer
  ( HttpServer(..)
  , mkHttpServer
  ) where

import Resolver.Models.AppSettings as AppSettings
import Resolver.Repositories.PoolRepository
import Resolver.Services.PoolResolver
import RIO
import Data.Int
import Control.Monad.Trans.Except
import Servant
import Network.Wai.Handler.Warp as Warp
import GHC.Natural
import ErgoDex.Amm.Pool
import CardanoTx.Models
import Explorer.Types
import Core.Types

data HttpServer f = HttpServer
  { runHttpServer :: IO ()
  }

mkHttpServer :: (MonadIO f) => HttpServerSettings -> PoolResolver f -> PoolRepository f -> UnliftIO f -> HttpServer f
mkHttpServer set resolver repo uIO = HttpServer $ runHttpServer' set resolver repo uIO

runHttpServer' :: (MonadIO f) => HttpServerSettings -> PoolResolver f -> PoolRepository f -> UnliftIO f -> IO ()
runHttpServer' HttpServerSettings{..} resolver repo uIO =
  (Warp.run (fromIntegral getPort) (httpApp resolver repo uIO))

type Api =""
  "resolve" :> ReqBody '[JSON] PoolId         :> Post '[JSON] (Maybe ConfirmedPool) :<|>
  "update"  :> ReqBody '[JSON] PredictedPool  :> Post '[JSON] ()

apiProxy :: Proxy Api
apiProxy = Proxy

f2Handler :: UnliftIO f -> f a -> Servant.Handler a
f2Handler UnliftIO{..} = liftIO . unliftIO

httpApp :: PoolResolver f -> PoolRepository f -> UnliftIO f -> Application
httpApp r p un = serve apiProxy $ hoistServer apiProxy (f2Handler un) (server r p)

server :: PoolResolver f -> PoolRepository f -> ServerT Api f
server r p =
  resolvePool r :<|>
  update p
 
resolvePool :: PoolResolver f -> PoolId -> f (Maybe ConfirmedPool)
resolvePool PoolResolver{..} = resolve

update :: PoolRepository f -> PredictedPool -> f ()
update PoolRepository{..} = putPredicted