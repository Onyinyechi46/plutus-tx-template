{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
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

module AuctionMintingPolicy where

import PlutusLedgerApi.V1.Value (flattenValue)
import PlutusLedgerApi.V2 (PubKeyHash, ScriptContext(..), TxInfo(..))
import PlutusLedgerApi.V2.Contexts (ownCurrencySymbol, txSignedBy)
import PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

------------------------------------------------------------
-- Type Synonyms
------------------------------------------------------------

type AuctionOwner = PubKeyHash
type MintingRedeemer = ()

------------------------------------------------------------
-- Typed Minting Policy
------------------------------------------------------------

{-# INLINEABLE auctionMintingValidator #-}
auctionMintingValidator :: AuctionOwner -> MintingRedeemer -> ScriptContext -> Bool
auctionMintingValidator ownerPKH _ ctx =
    signedByOwner PlutusTx.&& mintedExactlyOne
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner = txSignedBy info ownerPKH

    mintedExactlyOne :: Bool
    mintedExactlyOne = case flattenValue (txInfoMint info) of
      [(cs, _tn, amt)] ->
        cs PlutusTx.== ownCurrencySymbol ctx PlutusTx.&& amt PlutusTx.== 1
      _ -> False

------------------------------------------------------------
-- Untyped Minting Policy (for PlutusTx)
------------------------------------------------------------

{-# INLINEABLE untypedAuctionMintingPolicy #-}
untypedAuctionMintingPolicy :: AuctionOwner -> BuiltinData -> BuiltinData -> ()
untypedAuctionMintingPolicy owner redeemer ctx =
  PlutusTx.check $
    auctionMintingValidator
      owner
      (unsafeFromBuiltinData redeemer)
      (unsafeFromBuiltinData ctx)

------------------------------------------------------------
-- Compile the Minting Policy
------------------------------------------------------------

auctionMintingPolicyScript ::
  AuctionOwner ->
  CompiledCode (BuiltinData -> BuiltinData -> ())
auctionMintingPolicyScript owner =
  $$(PlutusTx.compile [||untypedAuctionMintingPolicy||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 owner
