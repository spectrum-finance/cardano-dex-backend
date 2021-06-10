{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dex.ModelsConverter (convertToPlutusTxOut) where

import RIO
import Dex.Models.ApiTxOut
import  Plutus.V1.Ledger.Tx (TxOut(..))
import qualified Plutus.V1.Ledger.Ada             as Ada
import           Plutus.V1.Ledger.Address
import qualified Plutus.V1.Ledger.Contexts        as Validation
import           Plutus.V1.Ledger.Credential      (Credential (..))
import           Plutus.V1.Ledger.Crypto
import qualified Plutus.V1.Ledger.Interval        as Interval
import           Plutus.V1.Ledger.Scripts
import qualified Plutus.V1.Ledger.Scripts         as Scripts
import qualified Plutus.V1.Ledger.Slot            as Slot
import           Plutus.V1.Ledger.TxId
import qualified Plutus.V1.Ledger.Value           as V
import qualified Data.ByteString as BS

import Data.Text.Encoding as E
import qualified RIO.Map as Map

convertToPlutusTxOut :: ApiTxOut -> TxOut
convertToPlutusTxOut ApiTxOut {
    txOutAddress = ApiAddr { 
        addressStakingCredential = addressStakingCredential,
        contents = ApiContents { getPubKeyHash = getPubKeyHash } },
    txOutValue = _,
    txOutDatumHash = txOutDatumHash
} = 
    let datumHash = fmap (DatumHash $ E.encodeUtf8) txOutDatumHash
        addrStaking = fmap (StakingCredential $ E.encodeUtf8) addressStakingCredential
        addrCredential = ScriptCredential $ ValidatorHash getPubKeyHash
        addr = Address addrCredential addrStaking
        value = Value Map.empty
    in TxOut addr value datumHash