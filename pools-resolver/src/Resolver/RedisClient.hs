module Resolver.RedisClient where

import RIO
import Resolver.Models.CfmmPool

getByPoolId :: PoolId -> Maybe CfmmPool
getByPoolId _ = undefined 