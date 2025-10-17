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
import PlutusLedgerApi.V1.Interval (contains, to, from)
import PlutusLedgerApi.V1.Value (lovelaceValueOf, valueOf)
import PlutusLedgerApi.V2 (CurrencySymbol, Datum (..), OutputDatum (..), ScriptContext (..),
                           TokenName, TxInfo (..), TxOut (..))
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs)
import PlutusTx
import PlutusTx.AsData qualified as PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude qualified as PlutusTx
import PlutusTx.Show qualified as PlutusTx
import PlutusTx.List qualified as List

-- | Auction parameters and data types -----------------------------------------------

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
  { bAddr   :: PlutusTx.BuiltinByteString -- Possibly unused? Consider removing if not needed.
  , bPkh    :: PubKeyHash
  , bAmount :: Lovelace
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.deriveShow ''Bid
PlutusTx.makeIsDataSchemaIndexed ''Bid [('Bid, 0)]

instance PlutusTx.Eq Bid where
  {-# INLINEABLE (==) #-}
  bid == bid' = bPkh bid PlutusTx.== bPkh bid' PlutusTx.&& bAmount bid PlutusTx.== bAmount bid'

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

-- | Utility helpers -----------------------------------------------------------------

{-# INLINEABLE txOutputs #-}
txOutputs :: TxInfo -> [TxOut]
txOutputs = txInfoOutputs

{-# INLINEABLE findOutputByPkh #-}
-- | Find an output that pays to the given PubKeyHash and satisfies the supplied predicate on Value.
findOutputByPkh :: PubKeyHash -> (PlutusTx.Value -> Bool) -> TxInfo -> Maybe TxOut
findOutputByPkh pkh predVal txi =
  List.find (\o -> toPubKeyHash (txOutAddress o) PlutusTx.== Just pkh PlutusTx.&& predVal (txOutValue o)) (txOutputs txi)

{-# INLINEABLE requires #-}
requires :: Bool -> PlutusTx.BuiltinString -> Bool
requires cond err = PlutusTx.traceIfFalse err cond

-- | Core validator (typed) -----------------------------------------------------------

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
      List.and
        [ requires (sufficientBid bid (AuctionDatum highestBid) params) "Bid too small"
        , requires (validBidTime params txInfo) "Bid submitted outside allowed time"
        , requires (refundsPreviousHighestBid highestBid txInfo) "Previous highest bid refund not found"
        , requires (correctContinuingOutput bid ctx params) "Continuing output invalid"
        ]
    Payout ->
      List.and
        [ requires (validPayoutTime params txInfo) "Payout attempted before end time"
        , requires (sellerGetsHighestBid highestBid txInfo params) "Seller did not receive highest bid"
        , requires (highestBidderGetsAsset highestBid txInfo params) "Highest bidder did not receive asset"
        ]

-- | Bid checks ----------------------------------------------------------------------

{-# INLINEABLE sufficientBid #-}
sufficientBid :: Bid -> AuctionDatum -> AuctionParams -> Bool
sufficientBid (Bid _ _ amt) (AuctionDatum maybeH) params = case maybeH of
  Just (Bid _ _ prevAmt) -> amt PlutusTx.> prevAmt
  Nothing                -> amt PlutusTx.>= apMinBid params

-- | Time checks ---------------------------------------------------------------------

{-# INLINEABLE validBidTime #-}
validBidTime :: AuctionParams -> TxInfo -> Bool
validBidTime params tx =
  -- New bids must be included in a tx whose valid range is before or containing the end time
  to (apEndTime params) `contains` txInfoValidRange tx

{-# INLINEABLE validPayoutTime #-}
validPayoutTime :: AuctionParams -> TxInfo -> Bool
validPayoutTime params tx =
  -- Payout must happen at or after end time
  from (apEndTime params) `contains` txInfoValidRange tx

-- | Refund / payout helpers ----------------------------------------------------------

{-# INLINEABLE refundsPreviousHighestBid #-}
refundsPreviousHighestBid :: Maybe Bid -> TxInfo -> Bool
refundsPreviousHighestBid Nothing _ = True
refundsPreviousHighestBid (Just (Bid _ bidderPkh amt)) txi =
  case findOutputByPkh bidderPkh (\v -> lovelaceValueOf v PlutusTx.== amt) txi of
    Just _  -> True
    Nothing -> PlutusTx.traceError "Refund to previous highest bidder not found"

{-# INLINEABLE sellerGetsHighestBid #-}
sellerGetsHighestBid :: Maybe Bid -> TxInfo -> AuctionParams -> Bool
sellerGetsHighestBid Nothing _ _ = True
sellerGetsHighestBid (Just (Bid _ _ amt)) txi params =
  -- Strict equality: seller must receive exactly the bid lovelace
  case findOutputByPkh (apSeller params) (\v -> lovelaceValueOf v PlutusTx.== amt) txi of
    Just _  -> True
    Nothing -> PlutusTx.traceError "Seller payment not found"

{-# INLINEABLE highestBidderGetsAsset #-}
highestBidderGetsAsset :: Maybe Bid -> TxInfo -> AuctionParams -> Bool
highestBidderGetsAsset mb txi params =
  let beneficiary = maybe (apSeller params) bPkh mb
  in case findOutputByPkh beneficiary (\v -> valueOf v (apCurrencySymbol params) (apTokenName params) PlutusTx.== 1) txi of
       Just _  -> True
       Nothing -> PlutusTx.traceError "Asset not paid to winner (or seller if no bids)"

-- | Continuing output checks for new bid ---------------------------------------------

{-# INLINEABLE correctContinuingOutput #-}
correctContinuingOutput :: Bid -> ScriptContext -> AuctionParams -> Bool
correctContinuingOutput bid ctx params =
  case getContinuingOutputs ctx of
    [o] ->
      case txOutDatum o of
        OutputDatum (Datum d) ->
          case PlutusTx.fromBuiltinData d of
            Just (AuctionDatum (Just bid')) ->
              PlutusTx.traceIfFalse "Continuing datum does not contain expected Bid" (bid PlutusTx.== bid')
              PlutusTx.&& checkOutputValue bid (txOutValue o) params
            Just (AuctionDatum Nothing) ->
              PlutusTx.traceError "Continuing datum: expected Just Bid, got Nothing"
            Nothing -> PlutusTx.traceError "Failed to decode continuing output datum"
        OutputDatumHash _ -> PlutusTx.traceError "Expected OutputDatum, got OutputDatumHash"
        NoOutputDatum      -> PlutusTx.traceError "Expected OutputDatum, got NoOutputDatum"
    os -> PlutusTx.traceError $ PlutusTx.appendString "Expected exactly one continuing output, got " (PlutusTx.show $ List.length os)

{-# INLINEABLE checkOutputValue #-}
-- | Check that the continuing output contains the bid lovelace and exactly one token.
checkOutputValue :: Bid -> PlutusTx.Value -> AuctionParams -> Bool
checkOutputValue bid outValue params =
  let lovOk = lovelaceValueOf outValue PlutusTx.== bAmount bid
      tokenOk = valueOf outValue (apCurrencySymbol params) (apTokenName params) PlutusTx.== 1
  in PlutusTx.traceIfFalse "Continuing output lovelace mismatch" lovOk
     PlutusTx.&& PlutusTx.traceIfFalse "Continuing output token mismatch" tokenOk

-- | Untyped wrapper and compilation --------------------------------------------------

{-# INLINEABLE auctionUntypedValidator #-}
auctionUntypedValidator ::
  AuctionParams ->
  BuiltinData ->
  BuiltinData ->
  BuiltinData ->
  PlutusTx.BuiltinUnit
auctionUntypedValidator params datum redeemer ctx =
  PlutusTx.check
    ( auctionTypedValidator
        params
        (PlutusTx.unsafeFromBuiltinData datum)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

auctionValidatorScript ::
  AuctionParams ->
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
auctionValidatorScript params =
  $$(PlutusTx.compile [|| auctionUntypedValidator ||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 params
