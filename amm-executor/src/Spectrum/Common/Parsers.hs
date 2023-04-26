module Spectrum.Common.Parsers where

import Text.Parsec 
  ( (<?>) )
import qualified Text.Parsec          as Parsec
import qualified Text.Parsec.Language as Parsec
import qualified Text.Parsec.String   as Parsec
import qualified Text.Parsec.Token    as Parsec

import qualified Data.ByteString.Char8 as BSC

import Control.Applicative 
  ( some )

import Cardano.Api.Shelley
  ( TxId,
    TxIn(..),
    AsType(AsTxId),
    TxIx(..),
    deserialiseFromRawBytesHex,
    Error(displayError) )

parseTxIn :: Parsec.Parser TxIn
parseTxIn = TxIn <$> parseTxId <*> (Parsec.char '#' *> parseTxIx)

parseTxId :: Parsec.Parser TxId
parseTxId = do
  str <- some Parsec.hexDigit <?> "transaction id (hexadecimal)"
  case deserialiseFromRawBytesHex AsTxId (BSC.pack str) of
    Right addr -> return addr
    Left e -> fail $ "Incorrect transaction id format: " ++ displayError e

parseTxIx :: Parsec.Parser TxIx
parseTxIx = TxIx . fromIntegral <$> decimal

decimal :: Parsec.Parser Integer
Parsec.TokenParser { Parsec.decimal = decimal } = Parsec.haskell