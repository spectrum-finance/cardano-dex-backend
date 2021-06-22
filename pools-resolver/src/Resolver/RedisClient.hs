module Resolver.RedisClient where

import RIO
import Resolver.Models.CfmmPool

getByPoolId :: PoolId -> Maybe Pool
getByPoolId _ = undefined 