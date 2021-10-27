module Tracker.Repository.ExplorerRepo where

import Prelude
import Explorer.Types
import Database.Redis as Redis
import Tracker.Models.AppSettings
import GHC.Natural as Natural
import Control.Monad.IO.Unlift

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Class
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource as Resource
import RIO
import Data.ByteString.UTF8 as BSU 
import Data.ByteString as BS

data ExplorerRepo f = ExplorerRepo
  { putMinIndex :: Gix -> f ()
  , getMinIndex :: f Gix
  }

mkExplorerRepo
  :: (Monad f, MonadIO f) 
  => RedisSettings
  -> ResourceT f (ExplorerRepo f)
mkExplorerRepo settings =
    explorerRepoR 
  where
    poolR         = mkConnectionPool settings
    explorerRepoR = fmap (\c -> ExplorerRepo (putMinIndex' c) (getMinIndex' c)) poolR


mkConnectionPool
  :: (Monad f, MonadIO f) 
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

-- getLastPredicted' :: Connection -> PoolId -> IO (Maybe (Predicted Pool))
-- getLastPredicted' conn pIdLast = do
--     res <- runRedis conn $ do
--         Redis.get $ mkLastPredictedKey pIdLast
--     _ <- print res
--     let resParsed = (unsafeFromEither res) >>= (\s -> (Json.decode $ LBS.fromStrict s) :: Maybe (Predicted Pool))
--     pure resParsed