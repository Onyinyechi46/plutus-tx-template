{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module TokenMintingPolicy where

import PlutusTx
import PlutusTx.Prelude
import PlutusLedgerApi.V2
import PlutusLedgerApi.V1.Contexts (txSignedBy)

-- | Parameters for the minting policy
data TokenMintingParams = TokenMintingParams
  { tmpOwner :: PubKeyHash
  , tmpTokenName :: TokenName
  }
PlutusTx.makeLift ''TokenMintingParams

{-# INLINABLE mkTokenMintingPolicy #-}
mkTokenMintingPolicy :: TokenMintingParams -> () -> ScriptContext -> Bool
mkTokenMintingPolicy params _ ctx =
  traceIfFalse "Unauthorized mint" signedByOwner
    && traceIfFalse "Mint amount mismatch" correctMintAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByOwner :: Bool
    signedByOwner = txSignedBy info (tmpOwner params)

    correctMintAmount :: Bool
    correctMintAmount =
      case flattenValue (txInfoMint info) of
        [(_, tn, amt)] -> tn == tmpTokenName params && amt == 1
        _ -> False

{-# INLINABLE tokenMintingPolicy #-}
tokenMintingPolicy :: TokenMintingParams -> MintingPolicy
tokenMintingPolicy params =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \p -> mkTokenMintingPolicy p ||])
      `PlutusTx.applyCode` PlutusTx.liftCode params

tokenMintingPolicyScript :: TokenMintingParams -> CompiledCode (BuiltinData -> BuiltinData -> ())
tokenMintingPolicyScript params = $$(PlutusTx.compile [|| \p -> mkTokenMintingPolicy p ||])
  `PlutusTx.applyCode` PlutusTx.liftCode params
