{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AuctionMintingPolicyTest where

import Test.QuickCheck
import PlutusLedgerApi.V2 (PubKeyHash)
import PlutusTx.Prelude qualified as PlutusTx

import AuctionMintingPolicy

-- | Mock a minimal ScriptContext suitable for testing minting behavior.
-- In a real test suite, you'd use Plutus emulator mock contexts.
mockMintingScriptContext :: ScriptContext
mockMintingScriptContext = 
  -- TODO: Replace this with your own builder or emulator mock.
  -- For now, assume you have a helper that builds a minimal valid ScriptContext.
  mkMockContextWithMintedAmount 1

-- | Property: The minting policy should only allow exactly one token to be minted.
property_oneTokenMinted :: PubKeyHash -> Property
property_oneTokenMinted pkh =
  forAll arbitrary $ \redeemer ->
    let ctx = mockMintingScriptContext
        result = auctionTypedMintingPolicy pkh redeemer ctx
    in counterexample
        ("Minting policy failed or minted incorrect amount.\nContext: " <> show ctx)
        (result && mintedExactlyOneToken ctx)

-- | Property: Minting more than one token should fail.
property_rejectMultipleMint :: PubKeyHash -> Property
property_rejectMultipleMint pkh =
  forAll arbitrary $ \redeemer ->
    let ctx = mkMockContextWithMintedAmount 2
        result = auctionTypedMintingPolicy pkh redeemer ctx
    in counterexample
        "Policy should reject minting more than one token"
        (not result)

-- | Group and run tests.
main :: IO ()
main = do
  putStrLn "Testing Auction Minting Policy..."
  quickCheck property_oneTokenMinted
  quickCheck property_rejectMultipleMint
