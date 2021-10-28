module Tracker.Repository.ExplorerRepo where

import Explorer.Types

import Tracker.Models.AppConfig

import Prelude
import Database.Redis               as Redis
import Data.ByteString.UTF8         as BSU 
import Data.ByteString              as BS
import Control.Monad.Trans.Resource
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Class
import RIO

data ExplorerRepo f = ExplorerRepo
  { putMinIndex :: Gix -> f ()
  , getMinIndex :: f Gix
  }

mkExplorerRepo
  :: (MonadIO f) 
  => RedisSettings
  -> ResourceT f (ExplorerRepo f)
mkExplorerRepo settings =
    explorerRepoR 
  where
    poolR         = mkConnectionPool settings
    explorerRepoR = fmap (\c -> ExplorerRepo (putMinIndex' c) (getMinIndex' c)) poolR


mkConnectionPool
  :: (MonadIO f) 
  => RedisSettings
  -> ResourceT f Connection
mkConnectionPool RedisSettings{..} =
  lift $ liftIO $ checkedConnect
    defaultConnectInfo 
      { connectHost = redisHost
      , connectPort = UnixSocket redisPort
      }

putMinIndex'
  :: (MonadIO f)
  => Connection
  -> Gix
  -> f ()
putMinIndex' conn index =
  void $ liftIO $ runRedis conn $ do
    Redis.set "min_index" (BSU.fromString $ show index)

getMinIndex'
  :: (MonadIO f)
  => Connection
  -> f Gix
getMinIndex' conn = liftIO $ do
  res <- runRedis conn $ Redis.get "min_index"
  pure $ getOrElse res (Gix 0)

-- todo log err
getOrElse :: Either c (Maybe BS.ByteString) -> Gix -> Gix
getOrElse input defaultInput =
  case input of
    Left err    -> defaultInput
    Right value ->
      case value of
        Just v ->
          let
            str = BSU.toString v
            int = read str :: Integer
          in
            Gix int
        _ -> defaultInput