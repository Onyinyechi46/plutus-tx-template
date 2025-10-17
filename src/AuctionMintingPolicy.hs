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
    PubKeyHash,
    ScriptContext (..),
    TxInfo (..)
  )
import PlutusLedgerApi.V2.Contexts (
    ownCurrencySymbol,
    txSignedBy
  )
import PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

--------------------------------------------------------------------------------
-- | TYPES
--------------------------------------------------------------------------------

-- | Parameter: the public key hash allowed to mint
type AuctionMintingParams = PubKeyHash

-- | Redeemer is unused for this simple minting policy
type AuctionMintingRedeemer = ()

--------------------------------------------------------------------------------
-- | ON-CHAIN VALIDATION LOGIC
--------------------------------------------------------------------------------

{-# INLINEABLE auctionTypedMintingPolicy #-}
-- | Ensures:
--   1. The transaction is signed by the given PubKeyHash
--   2. Exactly one token is minted by this policy
auctionTypedMintingPolicy ::
  AuctionMintingParams ->
  AuctionMintingRedeemer ->
  ScriptContext ->
  Bool
auctionTypedMintingPolicy pkh _ ctx =
    signedByOwner PlutusTx.&& exactlyOneTokenMinted
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner = txSignedBy info pkh

    exactlyOneTokenMinted :: Bool
    exactlyOneTokenMinted = case flattenValue (txInfoMint info) of
      [(cs, _tn, q)] ->
        cs PlutusTx.== ownCurrencySymbol ctx PlutusTx.&& q PlutusTx.== 1
      _ -> False

--------------------------------------------------------------------------------
-- | UNTYPED POLICY WRAPPER
--------------------------------------------------------------------------------

{-# INLINEABLE auctionUntypedMintingPolicy #-}
auctionUntypedMintingPolicy ::
  AuctionMintingParams ->
  BuiltinData ->
  BuiltinData ->
  PlutusTx.BuiltinUnit
auctionUntypedMintingPolicy pkh redeemer ctx =
  PlutusTx.check $
    auctionTypedMintingPolicy
      pkh
      (unsafeFromBuiltinData redeemer)
      (unsafeFromBuiltinData ctx)

--------------------------------------------------------------------------------
-- | COMPILED MINTING POLICY
--------------------------------------------------------------------------------

auctionMintingPolicyScript ::
  AuctionMintingParams ->
  CompiledCode (BuiltinData -> BuiltinData -> PlutusTx.BuiltinUnit)
auctionMintingPolicyScript pkh =
  $$(PlutusTx.compile [||auctionUntypedMintingPolicy||])
    `PlutusTx.unsafeApplyCode`
    PlutusTx.liftCode plcVersion100 pkh
