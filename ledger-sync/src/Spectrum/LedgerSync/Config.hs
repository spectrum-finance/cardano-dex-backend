module Spectrum.LedgerSync.Config
  ( LedgerSyncConfig(..)
  , NetworkParameters(..)
  , parseNetworkParameters
  ) where

import RIO ( (>=>) )
import RIO.FilePath (replaceFileName)

import System.Exit (die)
import GHC.Generics ( Generic )

import Dhall (FromDhall)

import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Pretty as Yaml

import Data.Aeson.Lens
  ( key, _Integer, _String )
import Data.Time.Format.ISO8601
  ( iso8601ParseM )
import Data.Profunctor.Unsafe
  ( ( #. ) )
import Data.Text.Encoding
  ( decodeUtf8 )
import qualified Data.Text as T
import Data.Monoid
  ( First (getFirst, First) )

import Control.Monad.Trans.Except
  ( ExceptT (ExceptT), runExceptT, withExceptT, throwE )
import Control.Applicative
  ( Const (getConst, Const) )

import Cardano.Chain.Slotting
  ( EpochSlots (..) )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SystemStart (..) )
import Ouroboros.Network.Magic
  ( NetworkMagic (..) )

import Spectrum.LedgerSync.Types ( StartingPoint )

data LedgerSyncConfig = LedgerSyncConfig
  { nodeSocket  :: !FilePath
  , maxInFlight :: !Int
  , startAt     :: !StartingPoint
  } deriving (Generic, FromDhall)

data NetworkParameters = NetworkParameters
  { networkMagic  :: !NetworkMagic
  , systemStart   :: !SystemStart
  , slotsPerEpoch :: !EpochSlots
  } deriving (Generic, Eq, Show)

parseNetworkParameters :: FilePath -> IO NetworkParameters
parseNetworkParameters configFile = runOrDie $ do
    config <- decodeYaml configFile
    let
      genesisFiles = (,)
        <$> config ^? key "ByronGenesisFile" . _String
        <*> config ^? key "ShelleyGenesisFile" . _String
    case genesisFiles of
      Nothing ->
        throwE "Missing 'ByronGenesisFile' and/or 'ShelleyGenesisFile' from Cardano's configuration?"
      Just (T.unpack -> byronGenesisFile, T.unpack -> shelleyGenesisFile) -> do
        byronGenesis   <- decodeYaml (replaceFileName configFile byronGenesisFile)
        shelleyGenesis <- decodeYaml (replaceFileName configFile shelleyGenesisFile)
        let
          params = (,,)
            <$> (shelleyGenesis ^? key "networkMagic" . _Integer)
            <*> (iso8601ParseM . T.unpack =<< shelleyGenesis ^? key "systemStart" . _String)
            <*> (byronGenesis ^? key "protocolConsts" . key "k" . _Integer)
        case params of
          Nothing -> do
            let prettyYaml = T.unpack $ decodeUtf8 (Yaml.encodePretty Yaml.defConfig shelleyGenesis)
            throwE $ unwords
              [ "Couldn't find (or failed to parse) required network"
              , "parameters (networkMagic, systemStart and/or epochLength)"
              , "in genesis file: \n" <> prettyYaml
              ]
          Just (nm, ss, k) ->
            return NetworkParameters
              { networkMagic =
                  NetworkMagic (fromIntegral nm)
              , systemStart =
                  SystemStart ss
              , slotsPerEpoch  =
                  EpochSlots (fromIntegral $ 10 * k)
              }
  where
    runOrDie :: ExceptT String IO a -> IO a
    runOrDie = runExceptT >=> either die pure

    prettyParseException :: Yaml.ParseException -> String
    prettyParseException e = "Failed to decode JSON (or YAML) file: " <> show e

    decodeYaml :: FilePath -> ExceptT String IO Yaml.Value
    decodeYaml = withExceptT prettyParseException . ExceptT . Yaml.decodeFileEither

infixl 8 ^?
(^?) :: s -> ((a -> Const (First a) a) -> s -> Const (First a) s) -> Maybe a
s ^? l = getFirst (fmof l (First #. Just) s)
  where fmof l' f = getConst #. l' (Const #. f)
