module Executor.Clients.PoolsResolverClient
  ( PoolsResolverClient(..)
  , mkPoolsResolverClient
  ) where

import           Executor.Models.Settings 
          
import           ErgoDex.State
import           ErgoDex.Amm.Pool

import qualified Prelude as F
import           RIO
import           Network.HTTP.Simple
import           Data.ByteString.Char8
import           GHC.Natural           

data PoolsResolverClient = PoolsResolverClient
  { resolvePoolReq :: PoolId         -> IO (Maybe (Confirmed Pool))
  , sendPredicted  :: Predicted Pool -> IO ()
  }

mkPoolsResolverClient :: PoolsResolverClientSettings -> PoolsResolverClient
mkPoolsResolverClient settings = 
  PoolsResolverClient (resolvePoolReq' settings) (sendPredicted' settings)

resolvePoolReq' :: PoolsResolverClientSettings -> PoolId -> IO (Maybe (Confirmed Pool))
resolvePoolReq' PoolsResolverClientSettings{..} poolId = do
  let request = defaultRequest
        & setRequestPath (pack "resolve") 
        & setRequestHost (pack getHost) 
        & setRequestPort (naturalToInt getPort)
        & setRequestMethod (pack "POST")
        & setRequestBodyJSON poolId
  response <- httpJSON request
  let result = getResponseBody response :: (Maybe (Confirmed Pool))
  F.print $ "HttpClient: got confirmed pool from network" ++ show result
  pure result 

sendPredicted' :: PoolsResolverClientSettings -> Predicted Pool -> IO ()
sendPredicted' PoolsResolverClientSettings{..} predicted = do
  let request = defaultRequest
        & setRequestPath (pack "update") 
        & setRequestHost (pack getHost) 
        & setRequestPort (naturalToInt getPort)
        & setRequestMethod (pack "POST")
        & setRequestBodyJSON predicted
  _ <- httpNoBody request
  F.print "HttpClient: sent predicted pool successfully"