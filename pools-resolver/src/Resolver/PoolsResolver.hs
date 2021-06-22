module Resolver.PoolsResolver (resolve) where

import RIO
import Resolver.Models.CfmmPool
import Resolver.Pool
    ( putPredicted,
      getLastPredicted,
      getLastConfirmed,
      existsPredicted )
import Prelude (print)

resolve :: PoolId -> IO (Maybe CfmmPool)
resolve poolId = do
    lastConfirmed <- getLastConfirmed poolId
    _             <- print lastConfirmed
    lastPredicted <- getLastPredicted poolId
    _             <- print lastPredicted
    process lastConfirmed lastPredicted

process :: Maybe CfmmPool -> Maybe CfmmPool -> IO (Maybe CfmmPool)
process confirmedMaybe predictedMaybe = do
    case (confirmedMaybe, predictedMaybe) of 
        (Just confirmed, Just predicted) -> do
            _ <- print "Both have Just type"
            let boxConfirmed = txOutRef confirmed
                boxPredicted = txOutRef predicted
                upToDate = lastConfirmedBoxGix boxConfirmed == lastConfirmedBoxGix boxPredicted
            consistentChain <- existsPredicted (poolId confirmed) (lastTxOutId boxConfirmed)
            fmap Just (if upToDate then pure predicted else pessimistic consistentChain predicted boxConfirmed confirmed)
        (Just confirmed, _) -> do
            _ <- print "Just only confirmed. Predicted is empty."
            pure $ Just confirmed
        _ -> do
            _ <- print "Both are nothing."
            pure Nothing
    
needToUpdate :: CfmmPool -> Int -> IO CfmmPool
needToUpdate (CfmmPool a b c d e (TxOutRef f _)) newGix = do
    let updatedPool = CfmmPool a b c d e (TxOutRef f newGix)
    _ <- putPredicted updatedPool
    pure updatedPool

pessimistic :: Bool -> CfmmPool -> TxOutRef -> CfmmPool -> IO CfmmPool 
pessimistic consistentChain predictedPool confirmedBox confirmedPool = do
    if consistentChain then needToUpdate predictedPool (lastConfirmedBoxGix confirmedBox) else pure confirmedPool
