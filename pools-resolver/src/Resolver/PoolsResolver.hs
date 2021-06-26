module Resolver.PoolsResolver 
    ( resolve
    ) where

import RIO
import Resolver.Pool
    ( putPredicted,
      getLastPredicted,
      getLastConfirmed,
      existsPredicted )
import Prelude (print)
import Dex.Models
import Dex.Models as P (Pool(..)) 
import Resolver.Models.CfmmPool

resolve :: PoolId -> IO (Maybe Pool)
resolve poolId = do
    lastConfirmed <- getLastConfirmed poolId
    _             <- print lastConfirmed
    lastPredicted <- getLastPredicted poolId
    _             <- print lastPredicted
    process lastConfirmed lastPredicted

process :: Maybe (ConfirmedPool Pool) -> Maybe (PredictedPool Pool) -> IO (Maybe Pool)
process confirmedMaybe predictedMaybe = do
    case (confirmedMaybe, predictedMaybe) of 
        (Just (ConfirmedPool confirmed), Just (PredictedPool predicted)) -> do
            _ <- print "Got confirmed and predicted pools in process function."
            let upToDate = gIx (gId (fullTxOut confirmed)) == gIx (gId (fullTxOut predicted))                
            consistentChain <- existsPredicted (P.poolId confirmed) (txOutRefId $ fullTxOut confirmed) (txOutRefIdx $ fullTxOut confirmed)
            fmap Just (if upToDate then pure predicted else pessimistic consistentChain (PredictedPool predicted) (ConfirmedPool confirmed))
        (Just (ConfirmedPool confirmed), _) -> do
            _ <- print "Just only confirmed. Predicted is empty."
            pure $ Just confirmed
        _ -> do
            _ <- print "Both are nothing."
            pure Nothing
    
needToUpdate :: PredictedPool Pool -> GId -> IO Pool
needToUpdate (PredictedPool (Pool a b (FullTxOut _ q w e r t))) newGix = do
    let updatedPool = PredictedPool $ Pool a b (FullTxOut newGix q w e r t)
    _ <- putPredicted updatedPool
    pure $ predicted updatedPool

pessimistic :: Bool -> PredictedPool Pool -> ConfirmedPool Pool -> IO Pool 
pessimistic consistentChain predictedPool confirmedPool = do
    if consistentChain then needToUpdate predictedPool (gId $ fullTxOut $ confirmed confirmedPool) else pure $ confirmed confirmedPool
