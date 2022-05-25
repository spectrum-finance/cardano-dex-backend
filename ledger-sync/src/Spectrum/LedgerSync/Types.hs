module Spectrum.LedgerSync.Types
  ( StartingPoint(..)
  , ConcreteHash(..)
  , toPoint
  ) where

import GHC.Generics ( Generic )

import qualified Data.Text as T

import Dhall (FromDhall)
import qualified Dhall         as D
import Dhall.Core ( Expr(..), Chunks(..) )

import Ouroboros.Consensus.Block ( SlotNo(SlotNo), Point (BlockPoint) )
import Ouroboros.Network.Block   ( HeaderHash, atSlot, withHash )
import Spectrum.LedgerSync.Protocol.Client ( Block )

import Cardano.Crypto.Hashing ( decodeHash, hashToBytes )
import Ouroboros.Consensus.HardFork.Combinator ( OneEraHash(OneEraHash) )
import Data.ByteString.Short ( toShort )
import Ouroboros.Consensus.Cardano.Block ( CardanoEras )

data StartingPoint = StartingPoint
  { slot :: SlotNo
  , hash :: ConcreteHash
  } deriving (Generic, Eq, Show, FromDhall)

toPoint :: StartingPoint -> Point Block
toPoint StartingPoint{slot, hash=ConcreteHash hash} = BlockPoint{atSlot=slot, withHash=hash}

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

oneEraHashFromString :: D.Text -> Either String (OneEraHash (CardanoEras crypto))
oneEraHashFromString =
  either (const $ Left "Invalid hash") (pure . OneEraHash . toShort . hashToBytes) . decodeHash
