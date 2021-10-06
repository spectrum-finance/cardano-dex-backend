module Resolver.Services.PoolsResolver 
    ( PoolResolver(..)
    , mkPoolResolver
    ) where

import RIO
import Resolver.Repositories.PoolRepository
import Prelude (print)
import Dex.Models
    ( Pool(Pool, poolData, fullTxOut),
      FullTxOut(FullTxOut, refId, refIdx, outGId),
      PoolData(poolId),
      GId(gIdx) )
import Resolver.Models.CfmmPool
import Utils (PoolId(..))

data PoolResolver = PoolResolver
    { resolve :: PoolId -> IO (Maybe Pool) 
    }

mkPoolResolver :: PoolRepository -> PoolResolver
mkPoolResolver p = PoolResolver $ resolve' p

resolve' :: PoolRepository -> PoolId -> IO (Maybe Pool)
resolve' p@PoolRepository{..} poolId = do
    lastConfirmed <- getLastConfirmed poolId
    _             <- print lastConfirmed
    lastPredicted <- getLastPredicted poolId
    _             <- print lastPredicted
    process p lastConfirmed lastPredicted

process :: PoolRepository -> Maybe (ConfirmedPool Pool) -> Maybe (PredictedPool Pool) -> IO (Maybe Pool)
process p@PoolRepository{..} confirmedMaybe predictedMaybe = do
    case (confirmedMaybe, predictedMaybe) of 
        (Just (ConfirmedPool confirmed), Just (PredictedPool predicted)) -> do
            _ <- print "Got confirmed and predicted pools in process function."
            let upToDate = (gIdx $ outGId (fullTxOut confirmed)) == (gIdx $ outGId (fullTxOut predicted))           
            consistentChain <- existsPredicted (poolId $ poolData confirmed) (refId $ fullTxOut confirmed) (refIdx $ fullTxOut confirmed)
            fmap Just (if upToDate then pure predicted else pessimistic p consistentChain (PredictedPool predicted) (ConfirmedPool confirmed))
        (Just (ConfirmedPool confirmed), _) -> do
            _ <- print "Just only confirmed. Predicted is empty."
            pure $ Just confirmed
        _ -> do
            _ <- print "Both are nothing."
            pure Nothing

pessimistic :: PoolRepository -> Bool -> PredictedPool Pool -> ConfirmedPool Pool -> IO Pool 
pessimistic p consistentChain predictedPool confirmedPool = do
    if consistentChain then needToUpdate p predictedPool (outGId $ fullTxOut $ confirmed confirmedPool) else pure $ confirmed confirmedPool

needToUpdate :: PoolRepository -> PredictedPool Pool -> GId -> IO Pool
needToUpdate PoolRepository{..} (PredictedPool (Pool b (FullTxOut _ q w e r t) s)) newGix = do
    let updatedPool = PredictedPool $ Pool b (FullTxOut newGix q w e r t) s
    _ <- putPredicted updatedPool
    pure $ predicted updatedPool
