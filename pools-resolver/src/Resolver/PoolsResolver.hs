module Resolver.PoolsResolver (resolve) where

import Resolver.RedisClient 
import RIO
import Resolver.Models.CfmmPool
import Resolver.Pool

resolve :: PoolId -> IO Maybe CfmmPool
resolve poolId = do
    lastConfirmed <- getLastConfirmed poolId
    _ <- print lastConfirmed
    lastPredicted <- getLastPredicted poolId
    _ <- print lastPredicted
    process lastConfirmed lastPredicted

process :: CfmmPool -> CfmmPool -> IO Maybe CfmmPool
process confirmed predicted = do
    let boxConfirmed = txOutRef confirmed
        boxPredicted = txOutRef predicted
        upToDate = lastConfirmedBoxGix boxConfirmed == lastConfirmedBoxGix boxPredicted
    _ <- case (boxConfirmed, boxPredicted) of 
        (Just a, Just b) -> do
            _ <- print "Both are Just."
            consistentChain <- existsPredicted (poolId confirmed) (lastTxOutId boxConfirmed)
            pool <- if consistentChain then needToUpdate predicted (lastConfirmedBoxGix boxConfirmed) else pure confirmed
            pure $ Just pool
        (Just a, _) -> do
            _ <- print "Just only confirmed. Predicted is empty."
            pure $ Just confirmed
        _ -> do
            _ <- print "Both are nothing."
            pure Nothing
    
needToUpdate :: CfmmPool -> Int -> IO CfmmPool
needToUpdate (CfmmPool a b c d e (TxOutRef a _)) newGix = do
    let updatedPool = CfmmPool a b c d e (TxOutRef a newGix)
    _ <- putPredicted updatedPool
    pure updatedPool