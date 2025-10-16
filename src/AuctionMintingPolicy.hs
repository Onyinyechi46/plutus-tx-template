{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
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

module AuctionMintingPolicy where

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V2 (
    PubKeyHash, ScriptContext(..), TxInfo(..),
    CurrencySymbol, TokenName
    )
import PlutusLedgerApi.V2.Contexts (
    ownCurrencySymbol, txSignedBy
    )
import PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

--------------------------------------------------------------------------------
-- Types

type AuctionMintingParams = PubKeyHash
type AuctionMintingRedeemer = ()

--------------------------------------------------------------------------------
-- Minting Policy Logic

{-# INLINEABLE auctionTypedMintingPolicy #-}
-- | Minting allowed only if:
--   1. The given PubKeyHash signed the transaction.
--   2. Exactly one token is minted under this policy.
auctionTypedMintingPolicy ::
  AuctionMintingParams ->
  AuctionMintingRedeemer ->
  ScriptContext ->
  Bool
auctionTypedMintingPolicy pkh _ ctx =
    txSignedBy info pkh PlutusTx.&& mintedExactlyOne
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    mintedExactlyOne :: Bool
    mintedExactlyOne =
      case flattenValue (txInfoMint info) of
        [(cs, _tn, q)] ->
            cs PlutusTx.== ownCurrencySymbol ctx PlutusTx.&& q PlutusTx.== 1
        _ -> False

--------------------------------------------------------------------------------
-- Untyped Wrapper

{-# INLINEABLE auctionUntypedMintingPolicy #-}
auctionUntypedMintingPolicy ::
  AuctionMintingParams ->
  BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit
auctionUntypedMintingPolicy pkh rawRedeemer rawCtx =
  PlutusTx.check $
    auctionTypedMintingPolicy
      pkh
      (PlutusTx.unsafeFromBuiltinData rawRedeemer)
      (PlutusTx.unsafeFromBuiltinData rawCtx)

--------------------------------------------------------------------------------
-- Compiled Script Export

auctionMintingPolicyScript ::
  AuctionMintingParams ->
  CompiledCode (BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
auctionMintingPolicyScript pkh =
  $$(PlutusTx.compile [|| \pkh' -> auctionUntypedMintingPolicy pkh' ||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 pkh
