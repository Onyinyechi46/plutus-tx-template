{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
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

import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, PubKeyHash)
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (lovelaceValueOf, valueOf)
import PlutusLedgerApi.V2 (CurrencySymbol, Datum (..), OutputDatum (..), ScriptContext (..),
                           TokenName, TxInfo (..), TxOut (..), from, to)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs)
import PlutusTx
import PlutusTx.AsData qualified as PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx
import PlutusTx.List qualified as List

{-|
  Auction parameters describing seller, token to auction, min bid and end time.
-}
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

-- | A recorded bid (address, bidder PKH, amount).
data Bid = Bid
  { bAddr   :: PlutusTx.BuiltinByteString
  , bPkh    :: PubKeyHash
  , bAmount :: Lovelace
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.deriveShow ''Bid
PlutusTx.makeIsDataSchemaIndexed ''Bid [('Bid, 0)]

instance PlutusTx.Eq Bid where
  {-# INLINEABLE (==) #-}
  b1 == b2 =
    (bPkh b1 PlutusTx.== bPkh b2) PlutusTx.&& (bAmount b1 PlutusTx.== bAmount b2)

newtype AuctionDatum = AuctionDatum { adHighestBid :: Maybe Bid }
  deriving stock (Generic)
  deriving newtype
    ( HasBlueprintDefinition
    , PlutusTx.ToData
    , PlutusTx.FromData
    , PlutusTx.UnsafeFromData
    )

data AuctionRedeemer = NewBid Bid | Payout
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''AuctionRedeemer [('NewBid, 0), ('Payout, 1)]

{-# INLINEABLE auctionTypedValidator #-}
auctionTypedValidator ::
  AuctionParams ->
  AuctionDatum ->
  AuctionRedeemer ->
  ScriptContext ->
  Bool
auctionTypedValidator params (AuctionDatum highestBidM) redeemer ctx@(ScriptContext txInfo _) =
  case redeemer of
    NewBid newBid ->
      PlutusTx.and4
        (sufficientBid newBid)
        validBidTime
        (refundsPreviousHighestBid highestBidM)
        (correctContinuingOutput newBid)
    Payout ->
      PlutusTx.and3
        validPayoutTime
        (sellerGetsHighestBid highestBidM)
        (highestBidderGetsAsset highestBidM)
  where
    -- Helpers for combining boolean checks (small convenience functions)
    {-# INLINEABLE andList #-}
    andList :: [Bool] -> Bool
    andList = List.foldl (\acc x -> acc PlutusTx.&& x) PlutusTx.True

    {-# INLINEABLE sufficientBid #-}
    sufficientBid :: Bid -> Bool
    sufficientBid (Bid _ _ amt) = case highestBidM of
      Just (Bid _ _ prevAmt) -> amt PlutusTx.> prevAmt
      Nothing                -> amt PlutusTx.>= apMinBid params

    {-# INLINEABLE validBidTime #-}
    -- Bid must be placed *before* end time: the tx validity range must be contained in (- , end]
    validBidTime :: Bool
    validBidTime = to (apEndTime params) `contains` txInfoValidRange txInfo

    {-# INLINEABLE refundsPreviousHighestBid #-}
    -- If there was a previous highest bid, there must be an output refunding that exact lovelace to the previous bidder PKH
    refundsPreviousHighestBid :: Maybe Bid -> Bool
    refundsPreviousHighestBid Nothing = True
    refundsPreviousHighestBid (Just (Bid _ prevPkh prevAmt)) =
      case List.find matchesRefund (txInfoOutputs txInfo) of
        Just _  -> True
        Nothing -> PlutusTx.traceError "refund to previous highest bidder not found"
      where
        matchesRefund o =
          toPubKeyHash (txOutAddress o) PlutusTx.== Just prevPkh
            PlutusTx.&& lovelaceValueOf (txOutValue o) PlutusTx.== prevAmt

    currencySymbol :: CurrencySymbol
    currencySymbol = apCurrencySymbol params

    tokenName :: TokenName
    tokenName = apTokenName params

    {-# INLINEABLE correctContinuingOutput #-}
    -- There must be exactly one continuing output (the updated script UTXO) that contains
    -- the new highest bid in its datum and holds exactly the expected values:
    --  - the lovelace funds equal to the bid amount
    --  - the token being auctioned (quantity = 1)
    correctContinuingOutput :: Bid -> Bool
    correctContinuingOutput bid = case getContinuingOutputs ctx of
      [o] ->
        case txOutDatum o of
          OutputDatum (Datum newDatum) ->
            case PlutusTx.fromBuiltinData newDatum of
              Just (AuctionDatum (Just bid')) ->
                let datumOk = PlutusTx.traceIfFalse "output datum contains unexpected bid" (bid PlutusTx.== bid')
                    val = txOutValue o
                    valueOk =
                      PlutusTx.traceIfFalse "output value does not match bid amount" (lovelaceValueOf val PlutusTx.== bAmount bid)
                        PlutusTx.&& PlutusTx.traceIfFalse "auction token not present in output" (valueOf val currencySymbol tokenName PlutusTx.== 1)
                 in datumOk PlutusTx.&& valueOk
              Just (AuctionDatum Nothing) -> PlutusTx.traceError "expected Just Bid in output datum but got Nothing"
              Nothing -> PlutusTx.traceError "failed to decode output datum"
          OutputDatumHash _ -> PlutusTx.traceError "expected concrete output datum (OutputDatum), got OutputDatumHash"
          NoOutputDatum -> PlutusTx.traceError "expected concrete output datum (OutputDatum), got NoOutputDatum"
      os -> PlutusTx.traceError (PlutusTx.concat ["expected exactly one continuing output, got ", PlutusTx.show (List.length os)])

    {-# INLINEABLE validPayoutTime #-}
    -- Payout can only happen at or after the end time
    validPayoutTime :: Bool
    validPayoutTime = from (apEndTime params) `contains` txInfoValidRange txInfo

    {-# INLINEABLE sellerGetsHighestBid #-}
    sellerGetsHighestBid :: Maybe Bid -> Bool
    sellerGetsHighestBid Nothing = True
    sellerGetsHighestBid (Just bid') =
      case List.find matchesSellerPayment (txInfoOutputs txInfo) of
        Just _  -> True
        Nothing -> PlutusTx.traceError "seller payment not found"
      where
        matchesSellerPayment o =
          toPubKeyHash (txOutAddress o) PlutusTx.== Just (apSeller params)
            PlutusTx.&& lovelaceValueOf (txOutValue o) PlutusTx.== bAmount bid'

    {-# INLINEABLE highestBidderGetsAsset #-}
    highestBidderGetsAsset :: Maybe Bid -> Bool
    highestBidderGetsAsset highestBidM' =
      let recipientPkh = case highestBidM' of
            Nothing   -> apSeller params -- if no bids, asset returns to seller
            Just bid' -> bPkh bid'
       in case List.find matchesRecipient (txInfoOutputs txInfo) of
            Just _  -> True
            Nothing -> PlutusTx.traceError "auction asset not paid to expected recipient"
      where
        matchesRecipient o =
          toPubKeyHash (txOutAddress o) PlutusTx.== Just recipientPkh
            PlutusTx.&& valueOf (txOutValue o) currencySymbol tokenName PlutusTx.== 1

-- Untyped wrapper used for on-chain compilation
{-# INLINEABLE auctionUntypedValidator #-}
auctionUntypedValidator ::
  AuctionParams ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  PlutusTx.BuiltinUnit
auctionUntypedValidator params datum redeemer ctx =
  PlutusTx.check $
    auctionTypedValidator
      params
      (PlutusTx.unsafeFromBuiltinData datum)
      (PlutusTx.unsafeFromBuiltinData redeemer)
      (PlutusTx.unsafeFromBuiltinData ctx)

auctionValidatorScript ::
  AuctionParams ->
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
auctionValidatorScript params =
  $$(PlutusTx.compile [||auctionUntypedValidator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params

-- Small example: alternate lightweight datatypes created via asData (kept for reference)
PlutusTx.asData
  [d|
    data Bid' = Bid'
      { bPkh' :: PubKeyHash
      , bAmount' :: Lovelace
      }
      deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

    data AuctionRedeemer' = NewBid' Bid | Payout'
      deriving newtype (Eq, Ord, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    |]
