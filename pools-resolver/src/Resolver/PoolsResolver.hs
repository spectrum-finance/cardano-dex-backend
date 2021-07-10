module Resolver.PoolsResolver 
    ( PoolResolver(..)
    , mkPoolResolver
    ) where

import RIO
import Resolver.Pool (PoolApi(..))
import Prelude (print)
import Dex.Models
import Resolver.Models.CfmmPool

data PoolResolver = PoolResolver
    { resolve :: PoolId -> IO (Maybe Pool) 
    }

mkPoolResolver :: PoolApi -> PoolResolver
mkPoolResolver p = PoolResolver $ resolve' p

resolve' :: PoolApi -> PoolId -> IO (Maybe Pool)
resolve' p@PoolApi{..} poolId = do
    lastConfirmed <- getLastConfirmed poolId
    _             <- print lastConfirmed
    lastPredicted <- getLastPredicted poolId
    _             <- print lastPredicted
    process p lastConfirmed lastPredicted

process :: PoolApi -> Maybe (ConfirmedPool Pool) -> Maybe (PredictedPool Pool) -> IO (Maybe Pool)
process p@PoolApi{..} confirmedMaybe predictedMaybe = do
    case (confirmedMaybe, predictedMaybe) of 
        (Just (ConfirmedPool confirmed), Just (PredictedPool predicted)) -> do
            _ <- print "Got confirmed and predicted pools in process function."
            let upToDate = gIdx (gId confirmed) == gIdx (gId predicted)              
            consistentChain <- existsPredicted (poolId $ poolData confirmed) (txOutRefId $ fullTxOut confirmed) (txOutRefIdx $ fullTxOut confirmed)
            fmap Just (if upToDate then pure predicted else pessimistic p consistentChain (PredictedPool predicted) (ConfirmedPool confirmed))
        (Just (ConfirmedPool confirmed), _) -> do
            _ <- print "Just only confirmed. Predicted is empty."
            pure $ Just confirmed
        _ -> do
            _ <- print "Both are nothing."
            pure Nothing

pessimistic :: PoolApi -> Bool -> PredictedPool Pool -> ConfirmedPool Pool -> IO Pool 
pessimistic p consistentChain predictedPool confirmedPool = do
    if consistentChain then needToUpdate p predictedPool (gId $ confirmed confirmedPool) else pure $ confirmed confirmedPool

needToUpdate :: PoolApi -> PredictedPool Pool -> GId -> IO Pool
needToUpdate PoolApi{..} (PredictedPool (Pool _ b (FullTxOut q w e r t))) newGix = do
    let updatedPool = PredictedPool $ Pool newGix b (FullTxOut q w e r t)
    _ <- putPredicted updatedPool
    pure $ predicted updatedPool
