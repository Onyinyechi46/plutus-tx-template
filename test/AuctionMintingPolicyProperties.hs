{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Test.QuickCheck
import           Ledger (PubKeyHash)
import           Plutus.V2.Ledger.Api (ScriptContext)
import           AuctionMintingPolicy (auctionTypedMintingPolicy, mockMintingScriptContext, mintedExactlyOneToken)

-- | Property: the auction minting policy should only mint exactly one token.
property_oneTokenMinted :: PubKeyHash -> Property
property_oneTokenMinted pkh =
    let
        redeemer :: ()      -- explicit type for clarity
        redeemer = ()
        context  :: ScriptContext
        context  = mockMintingScriptContext pkh
        result   = auctionTypedMintingPolicy pkh redeemer context
    in
        counterexample "Minting policy did not produce exactly one token" $
        result ==> mintedExactlyOneToken context

-- | Custom Arbitrary instance for PubKeyHash (for reproducible testing)
instance Arbitrary PubKeyHash where
    arbitrary = do
        bs <- vectorOf 28 arbitrary
        pure (toPubKeyHash bs)
      where
        toPubKeyHash = error "Define toPubKeyHash to match your PubKeyHash constructor"

main :: IO ()
main = do
    putStrLn "🔍 Testing: Auction Minting Policy - One Token Minted Property"
    quickCheck property_oneTokenMinted
