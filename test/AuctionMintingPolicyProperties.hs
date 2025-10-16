{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.QuickCheck
import AuctionMintingPolicy

-- | Property: Only one token should be minted per auction
-- We test that the policy allows minting exactly one token.
property_oneTokenMinted :: PubKeyHash -> Bool
property_oneTokenMinted pkh =
    let redeemer = ()
        ctx = mockMintingScriptContext
    in auctionTypedMintingPolicy pkh redeemer ctx
       && mintedExactlyOneToken ctx

-- | Run QuickCheck to test our property
main :: IO ()
main = do
  putStrLn "Testing Auction Minting Policy..."
  quickCheck property_oneTokenMinted
