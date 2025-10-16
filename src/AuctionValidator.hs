{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module AuctionValidator where

-- Imports
import GHC.Generics (Generic)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, PubKeyHash)
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (lovelaceValueOf, valueOf)
import PlutusLedgerApi.V2 (
  CurrencySymbol, Datum (..), OutputDatum (..), ScriptContext (..),
  TokenName, TxInfo (..), TxOut (..), from, to
)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs)
import PlutusCore.Version (plcVersion100)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import PlutusTx.Show qualified as PlutusTx
import PlutusTx.List qualified as List
import PlutusTx.Blueprint

------------------------------------------------------------
-- Data Types
------------------------------------------------------------

data AuctionParams = AuctionParams
  { apSeller         :: PubKeyHash
  , apCurrencySymbol :: CurrencySymbol
  , apTokenName      :: TokenName
  , apMinBid         :: Lovelace
  , apEndTime        :: POSIXTime
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeLift ''AuctionParams
PlutusTx.makeIsDataSchemaIndexed ''AuctionParams [('AuctionParams, 0)]

data Bid = Bid
  { bAddr   :: BuiltinByteString
  , bPkh    :: PubKeyHash
  , bAmount :: Lovelace
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.deriveShow ''Bid
PlutusTx.makeIsDataSchemaIndexed ''Bid [('Bid, 0)]

instance Eq Bid where
  {-# INLINEABLE (==) #-}
  a == b = bPkh a == bPkh b && bAmount a == bAmount b

newtype AuctionDatum = AuctionDatum { adHighestBid :: Maybe Bid }
  deriving stock (Generic)
  deriving newtype
    (HasBlueprintDefinition, ToData, FromData, UnsafeFromData)

data AuctionRedeemer = NewBid Bid | Payout
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''AuctionRedeemer [('NewBid, 0), ('Payout, 1)]

------------------------------------------------------------
-- On-Chain Validator Logic
------------------------------------------------------------

{-# INLINEABLE auctionTypedValidator #-}
auctionTypedValidator ::
  AuctionParams ->
  AuctionDatum ->
  AuctionRedeemer ->
  ScriptContext ->
  Bool
auctionTypedValidator params (AuctionDatum highestBid) redeemer ctx@(ScriptContext txInfo _) =
  case redeemer of
    NewBid bid ->
      traceIfFalse "Insufficient bid" (sufficientBid bid) &&
      traceIfFalse "Bid too late" validBidTime &&
      traceIfFalse "Refund missing" refundsPreviousHighestBid &&
      traceIfFalse "Invalid output" (correctOutput bid)
    Payout ->
      traceIfFalse "Too early to payout" validPayoutTime &&
      traceIfFalse "Seller not paid" sellerGetsHighestBid &&
      traceIfFalse "Winner not paid" highestBidderGetsAsset
  where
    ------------------------------------------------------------
    -- NewBid checks
    ------------------------------------------------------------
    sufficientBid :: Bid -> Bool
    sufficientBid (Bid _ _ amt) = case highestBid of
      Just (Bid _ _ amt') -> amt > amt'
      Nothing             -> amt >= apMinBid params

    validBidTime :: Bool
    validBidTime = to (apEndTime params) `contains` txInfoValidRange txInfo

    refundsPreviousHighestBid :: Bool
    refundsPreviousHighestBid = case highestBid of
      Nothing -> True
      Just (Bid _ bidderPkh amt) ->
        any
          (\o -> toPubKeyHash (txOutAddress o) == Just bidderPkh &&
                 lovelaceValueOf (txOutValue o) == amt)
          (txInfoOutputs txInfo)

    correctOutput :: Bid -> Bool
    correctOutput bid = case getContinuingOutputs ctx of
      [o] ->
        let correctDatum = case txOutDatum o of
              OutputDatum (Datum d) -> case fromBuiltinData d of
                Just (AuctionDatum (Just bid')) -> bid == bid'
                _ -> False
              _ -> False
            outValue = txOutValue o
            correctValue =
              lovelaceValueOf outValue == bAmount bid &&
              valueOf outValue (apCurrencySymbol params) (apTokenName params) == 1
        in correctDatum && correctValue
      _ -> traceError "Expected one continuing output"

    ------------------------------------------------------------
    -- Payout checks
    ------------------------------------------------------------
    validPayoutTime :: Bool
    validPayoutTime = from (apEndTime params) `contains` txInfoValidRange txInfo

    sellerGetsHighestBid :: Bool
    sellerGetsHighestBid = case highestBid of
      Nothing -> True
      Just bid ->
        any
          (\o -> toPubKeyHash (txOutAddress o) == Just (apSeller params) &&
                 lovelaceValueOf (txOutValue o) == bAmount bid)
          (txInfoOutputs txInfo)

    highestBidderGetsAsset :: Bool
    highestBidderGetsAsset =
      let recipient = maybe (apSeller params) bPkh highestBid
      in any
        (\o -> toPubKeyHash (txOutAddress o) == Just recipient &&
               valueOf (txOutValue o) (apCurrencySymbol params) (apTokenName params) == 1)
        (txInfoOutputs txInfo)

------------------------------------------------------------
-- Untyped Validator
------------------------------------------------------------

{-# INLINEABLE auctionUntypedValidator #-}
auctionUntypedValidator ::
  AuctionParams ->
  BuiltinData -> BuiltinData -> BuiltinData ->
  BuiltinUnit
auctionUntypedValidator params datum redeemer ctx =
  check $ auctionTypedValidator
    params
    (unsafeFromBuiltinData datum)
    (unsafeFromBuiltinData redeemer)
    (unsafeFromBuiltinData ctx)

auctionValidatorScript ::
  AuctionParams ->
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit)
auctionValidatorScript params =
  $$(compile [|| auctionUntypedValidator ||])
    `unsafeApplyCode` liftCode plcVersion100 params

------------------------------------------------------------
-- Optional: Simplified Types for External Use
------------------------------------------------------------

PlutusTx.asData
  [d|
    data Bid' = Bid'
      { bPkh'    :: PubKeyHash
      , bAmount' :: Lovelace
      }
      deriving newtype (Eq, Ord, ToData, FromData, UnsafeFromData)

    data AuctionRedeemer' = NewBid' Bid | Payout'
      deriving newtype (Eq, Ord, ToData, FromData, UnsafeFromData)
  |]
