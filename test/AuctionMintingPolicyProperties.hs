import Test.QuickCheck
import AuctionMintingPolicy

-- Property: The minting policy should mint exactly one token when used correctly
property_oneTokenMinted :: PubKeyHash -> Property
property_oneTokenMinted pkh =
  let
    redeemer = ()
    context  = mockMintingScriptContext pkh
  in
    auctionTypedMintingPolicy pkh redeemer context ==> mintedExactlyOneToken context

-- Run QuickCheck test
main :: IO ()
main = quickCheck property_oneTokenMinted
