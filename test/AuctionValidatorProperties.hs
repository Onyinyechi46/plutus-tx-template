{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import AuctionValidator -- Assume this contains necessary types and functions

-- Arbitrary instance for Bid ensuring positive bid amounts
instance Arbitrary Bid where
    arbitrary = do
        bAddress <- arbitrary
        bPubKey  <- arbitrary
        bAmount  <- arbitrary `suchThat` (> 0)
        return $ Bid bAddress bPubKey bAmount

-- Property: A new bid must be higher than the previous bid to be valid
prop_newBidMustBeHigher :: Bid -> Bid -> Property
prop_newBidMustBeHigher previousBid newBid =
    (bAmount newBid > bAmount previousBid) ==>
        let params   = AuctionParams "seller" "currencySymbol" "MY_TOKEN" 100 1725227091000
            datum    = AuctionDatum (Just previousBid)
            redeemer = NewBid newBid
            context  = mockScriptContext params datum redeemer -- Assuming this builds a valid mock context
        in auctionTypedValidator params datum redeemer context === True

main :: IO ()
main = quickCheck prop_newBidMustBeHigher
