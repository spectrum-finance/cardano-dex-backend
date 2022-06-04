{-# LANGUAGE DerivingVia #-}

module Spectrum.Executor.Types
  ( ConcretePoint(..)
  , ConcreteHash(..)
  , toPoint
  , fromPoint
  ) where

import GHC.Generics ( Generic )

import qualified Data.Text as T

import Data.ByteString.Short
  ( toShort, fromShort )
import Data.ByteString.Base16
  ( encodeBase16 )
import Data.Aeson
  ( ToJSON(..), FromJSON(..) )
import Data.Aeson.Types
  ( Value(String) )


import qualified Dhall as D
import Dhall
  ( FromDhall )
import Dhall.Core
  ( Expr(..), Chunks(..) )

import Ouroboros.Consensus.Block
  ( SlotNo(SlotNo), Point (BlockPoint) )
import Ouroboros.Network.Block
  ( HeaderHash, atSlot, withHash )
import Spectrum.LedgerSync.Protocol.Client
  ( Block )

import Cardano.Crypto.Hashing
  ( decodeHash, hashToBytes )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
  ( getOneEraHash )
import Ouroboros.Consensus.HardFork.Combinator
  ( OneEraHash(OneEraHash) )
import Ouroboros.Consensus.Cardano.Block
  ( CardanoEras )

data ConcretePoint = ConcretePoint
  { slot :: SlotNo
  , hash :: ConcreteHash
  } deriving (Generic, Eq, Show, FromDhall, ToJSON, FromJSON)

toPoint :: ConcretePoint -> Point Block
toPoint ConcretePoint{slot, hash=ConcreteHash hash} = BlockPoint{atSlot=slot, withHash=hash}

fromPoint :: Point Block -> ConcretePoint
fromPoint BlockPoint{atSlot, withHash} = ConcretePoint{slot=atSlot, hash=ConcreteHash withHash}

instance FromDhall SlotNo where
  autoWith _ = D.Decoder{..}
    where
      extract (NaturalLit nat) = pure $ SlotNo $ fromIntegral nat
      extract expr             = D.typeError expected expr

      expected = pure Natural

newtype ConcreteHash = ConcreteHash (HeaderHash Block)
  deriving newtype (Eq, Show)

instance FromDhall ConcreteHash where
  autoWith _ = D.Decoder{..}
    where
      extract (TextLit (Chunks [] t)) = either (D.extractError . T.pack) (pure . ConcreteHash) $ oneEraHashFromString t
      extract expr                    = D.typeError expected expr

      expected = pure Text

instance ToJSON ConcreteHash where
  toJSON (ConcreteHash hh) = toJSON . encodeBase16 . fromShort . getOneEraHash $ hh

instance FromJSON ConcreteHash where
  parseJSON (String s) = either fail (pure . ConcreteHash) $ oneEraHashFromString s
  parseJSON _          = fail "Expected a string"

oneEraHashFromString :: D.Text -> Either String (OneEraHash (CardanoEras crypto))
oneEraHashFromString =
  either (const $ Left "Invalid hash") (pure . OneEraHash . toShort . hashToBytes) . decodeHash
