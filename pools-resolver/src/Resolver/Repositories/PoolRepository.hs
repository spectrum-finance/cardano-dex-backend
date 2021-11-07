module Resolver.Repositories.PoolRepository
    ( PoolRepository(..)
    , mkPoolRepository
    ) where

import RIO
import Plutus.V1.Ledger.TxId
import RIO.ByteString.Lazy as BS
import Data.Aeson as Json
import Database.Redis as Redis
import Prelude (print)
import Data.ByteString.UTF8 as BSU
import RIO.ByteString.Lazy as LBS
import Resolver.Utils
import Resolver.Models.AppSettings (RedisSettings(..))
import ErgoDex.Amm.Pool
import ErgoDex.State
import Cardano.Types
import Cardano.Models
import Plutus.V1.Ledger.Tx

data PoolRepository = PoolRepository
    { putPredicted     :: Predicted Pool -> IO ()
    , putConfirmed     :: Confirmed Pool -> IO ()
    , getLastPredicted :: PoolId         -> IO (Maybe (Predicted Pool))
    , getLastConfirmed :: PoolId         -> IO (Maybe (Confirmed Pool))
    , existsPredicted  :: PoolId         -> IO Bool
    }

mkPoolRepository :: RedisSettings -> IO PoolRepository
mkPoolRepository redis = do
    conn <- checkedConnect $ defaultConnectInfo { connectHost = getRedisHost redis }
    _ <- print "Redis connection established..."
    pure $ PoolRepository (putPredicted' conn) (putConfirmed' conn) (getLastPredicted' conn) (getLastConfirmed' conn) (existsPredicted' conn)

putPredicted' :: Connection -> Predicted Pool -> IO ()
putPredicted' conn r@(Predicted TxOutCandidate{..} pool@Pool{..}) = do 
    res <- runRedis conn $ do
        let predictedNext = mkPredicted poolId
            predictedLast = mkLastPredictedKey poolId
            encodedPool = (BS.toStrict . encode) r
        Redis.set predictedNext encodedPool
        Redis.set predictedLast encodedPool
    print res

putConfirmed' :: Connection -> Confirmed Pool -> IO ()
putConfirmed' conn r@(Confirmed _ pool@Pool{..}) = do
    res <- runRedis conn $ do
        let confirmed = mkLastConfirmedKey poolId
            encodedPool = (BS.toStrict . encode) r
        Redis.set confirmed encodedPool
    print res

getLastPredicted' :: Connection -> PoolId -> IO (Maybe (Predicted Pool))
getLastPredicted' conn pIdLast = do
    res <- runRedis conn $ do
        Redis.get $ mkLastPredictedKey pIdLast
    _ <- print res
    let resParsed = (unsafeFromEither res) >>= (\s -> (Json.decode $ LBS.fromStrict s) :: Maybe (Predicted Pool))
    pure resParsed

getLastConfirmed' :: Connection -> PoolId -> IO (Maybe (Confirmed Pool))
getLastConfirmed' conn pIdConfirmed = do
    res <- runRedis conn $ do
        Redis.get $ mkLastConfirmedKey pIdConfirmed
    _ <- print res
    let resParsed = (unsafeFromEither res) >>= (\s -> (Json.decode $ LBS.fromStrict s) :: Maybe (Confirmed Pool))
    pure resParsed

existsPredicted' :: Connection -> PoolId -> IO Bool
existsPredicted' conn pId = do
    res <- runRedis conn $ do
        Redis.exists $ mkPredicted pId
    pure $ unsafeFromEither res

-------------------------------------------------------------------------------------

mkLastPredictedKey :: PoolId -> BSU.ByteString
mkLastPredictedKey (PoolId poolId) = BSU.fromString $ "predicted:last:" ++ show poolId

mkLastConfirmedKey :: PoolId -> BSU.ByteString
mkLastConfirmedKey (PoolId poolId) = BSU.fromString $ "confirmed:last:" ++ show poolId

mkPredicted :: PoolId -> BSU.ByteString
mkPredicted (PoolId pid) = BSU.fromString $ "predicted:" ++ show pid