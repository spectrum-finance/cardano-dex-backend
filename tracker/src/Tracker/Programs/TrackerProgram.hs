module Tracker.Programs.TrackerProgram where

import qualified Streamly.Prelude as S
import RIO 
import Tracker.Services.ExplorerService
import Tracker.Services.KafkaService
import Tracker.Utils
import ErgoDex.Class
import ErgoDex.Amm.Orders
import ErgoDex.Amm.Pool
import ErgoDex.State
import Data.Aeson
import qualified RIO.ByteString.Lazy as BL
import GHC.Natural as Natural
import Core.Streaming

import RIO
import Plutus.V1.Ledger.Address 
import Plutus.V1.Ledger.TxId 
import Plutus.V1.Ledger.Credential
import Plutus.V1.Ledger.Scripts
import Data.ByteString.Char8 as DataC
import qualified PlutusTx.AssocMap as AssocMap
import RIO.List as List
import RIO.Map as Map
import Plutus.V1.Ledger.Value
import Prelude as Prelude
import Cardano.Models
import Ledger.Tx
import PlutusTx.Builtins.Internal

data TrackerProgram f = TrackerProgram 
  { run :: f () 
  }

mkTrackerProgram 
  :: (Monad f) 
  => ExplorerProgrammSettings
  -> ExplorerService 
  -> Producer f String ConfirmedOrderEvent 
  -> Producer f String ConfirmedPoolEvent 
  -> TrackerProgram f
mkTrackerProgram settings explorer orderProd poolProd = 
  TrackerProgram $ run' settings explorer orderProd poolProd

run' 
  :: (Monad f) 
  => ExplorerProgrammSettings
  -> ExplorerService 
  -> Producer f String ConfirmedOrderEvent
  -> Producer f String ConfirmedPoolEvent
  -> f ()
run' ExplorerProgrammSettings{..} explorer orderProd poolProd =
    S.repeatM (threadDelay $ Natural.naturalToInt pollTime) >> process e k --todo threadDelay replace with streamly api
  -- & --todo err handle
  & S.drain

process 
  :: ExplorerService 
  -> Producer f String ConfirmedOrderEvent
  -> Producer f String ConfirmedPoolEvent 
  -> f ()
process ExplorerService{..} orderProd poolProd = do
  fulltxOuts <- getOutputs
  unspent    <- mapM toFullTxOut fulltxOuts
  let
    confirmedOrderEvents = 
        pairToOrderEvent confirmedSwaps ++ pairToOrderEvent confirmedDeposits ++ pairToOrderEvent confirmedRedeems
      where
        confirmedSwaps    = parseAmm :: [(Confirmed Swap, Integer)]
        confirmedDeposits = parseAmm :: [(Confirmed Deposit, Integer)]
        confirmedRedeems  = parseAmm :: [(Confirmed Redeem, Integer)]
  
    confirmedPoolEvents =
        pairToPoolEvent confirmedPools
      where
        confirmedPools = parseAmm :: [(Confirmed Pool, Integer)]
  
  _ <- produce orderProd confirmedOrderEvents
  _ <- produce poolProd confirmedPoolEvents
  pure ()
  -- print $ "TrackerProgramm::newAmmOutputsLength=" ++ show (length res1)
  -- print $ "TrackerProgramm::newProxyOutputs=" ++ show (length msgs)
  -- unless (null res1) (sendAmm res1 >> print "TrackerProgramm::AmmSuccessfullySentIntoKafka")
  -- unless (null msgs) (sendProxy msgs >> print "TrackerProgramm::ProxySuccessfullySentIntoKafka")

parseAmm
  :: (FromLedger amm)
  => [FullTxOut]
  -> [(Confirmed amm, Integer)]
parseAmm unspent = mapMaybe (\out@FullTxOut{..} -> parseFromLedger out >>= (\r -> (r, globalIndex))) unspent

pairToOrderEvent :: [(Confirmed a, Integer)] -> [ConfirmedOrderEvent]
pairToEvent pairs = 
  fmap (\((Confirmed out order), gix) -> ConfirmedOrderEvent $ (AnyOrder order) out gix) pairs

pairToPoolEvent :: [(Confirmed Pool, Integer)] -> [ConfirmedPoolEvent]
pairToPoolEvent pairs = 
  fmap (\((Confirmed out pool), gix) -> ConfirmedPoolEvent $ pool out gix) pairs

toFullTxOut :: ApiFullTxOut -> IO FullTxOut
toFullTxOut ApiFullTxOut{..} = do
  let value = Value $ AssocMap.fromList $ List.map (\OutputAsset{..} -> (CurrencySymbol $ BuiltinByteString $ DataC.pack policy, AssocMap.singleton (TokenName $ BuiltinByteString $ DataC.pack name) quantity)) assets
      (refId', refIdxRaw) = List.span (/= ':') ref
      refIdx' = List.drop 1 refIdxRaw
      res = FullTxOut 
        { fullTxOutGix     = Gix index
        , fullTxOutRef     = TxOutRef (TxId $ BuiltinByteString $ DataC.pack refId') (read refIdx' :: Integer)
        , fullTxOutAddress = scriptHashAddress (ValidatorHash $ BuiltinByteString $ DataC.pack addr)
        , fullTxOutValue   = value
        , fullTxOutDatum   = data'
        }
  pure res